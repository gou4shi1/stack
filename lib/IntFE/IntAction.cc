#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/CodeGen/CodeGenAction.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendPluginRegistry.h>
#include <clang/Frontend/MultiplexConsumer.h>
#include <clang/Lex/Preprocessor.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/Bitcode/BitcodeWriter.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <set>

using namespace clang;
using namespace llvm;

// Filename, Line => Location.
typedef StringMap<DenseMap<unsigned, std::set<SourceLocation>>> MacroMap;

namespace {

// Add macro-expanded code with if or ?: conditions.
class ExtractMacroVisitor : public RecursiveASTVisitor<ExtractMacroVisitor> {
  public:
    ExtractMacroVisitor(SourceManager &SM, MacroMap &MM) : SM(SM), MM(MM) {}

    bool VisitIfStmt(IfStmt *S) {
        addMacroLoc(S->getBeginLoc());
        return true;
    }

    bool VisitConditionalOperator(ConditionalOperator *E) {
        addMacroLoc(E->getBeginLoc());
        return true;
    }

    // GNU extension ?:, the middle operand omitted.
    bool VisitBinaryConditionalOperator(BinaryConditionalOperator *E) {
        addMacroLoc(E->getBeginLoc());
        return true;
    }

    // p && p->x.
    bool VisitBinaryOperator(BinaryOperator *E) {
        if (E->isLogicalOp() || E->isEqualityOp())
            addMacroLoc(E->getBeginLoc());
        return true;
    }

  private:
    SourceManager &SM;
    MacroMap &MM;

    void addMacroLoc(SourceLocation Loc) {
        if (Loc.isInvalid())
            return;
        if (!Loc.isMacroID())
            return;
        PresumedLoc PLoc = SM.getPresumedLoc(Loc);
        if (PLoc.isInvalid())
            return;
        MM[PLoc.getFilename()][PLoc.getLine()].insert(Loc);
    }
};

class ExtractMacroConsumer : public ASTConsumer {
  public:
    ExtractMacroConsumer(MacroMap &MM) : MM(MM) {}

    virtual void HandleTranslationUnit(ASTContext &Ctx) override {
        ExtractMacroVisitor Visitor(Ctx.getSourceManager(), MM);
        Visitor.TraverseDecl(Ctx.getTranslationUnitDecl());
    }

  private:
    MacroMap &MM;
};

// Use multiple inheritance to intercept code generation.
class IntAction : public PluginASTAction, EmitLLVMOnlyAction {
    typedef PluginASTAction super;
    typedef EmitLLVMOnlyAction Delegate;

  public:
    virtual bool ParseArgs(const CompilerInstance &,
                           const std::vector<std::string> &) override {
        return true;
    }

    virtual bool usesPreprocessorOnly() const override {
        return Delegate::usesPreprocessorOnly();
    }

    virtual TranslationUnitKind getTranslationUnitKind() override {
        return Delegate::getTranslationUnitKind();
    }

    virtual bool hasPCHSupport() const override {
        return Delegate::hasPCHSupport();
    }

    virtual bool hasASTFileSupport() const override {
        return Delegate::hasASTFileSupport();
    }

    virtual bool hasIRSupport() const override {
        return Delegate::hasIRSupport();
    }

    virtual bool hasCodeCompletionSupport() const override {
        return Delegate::hasCodeCompletionSupport();
    }

  protected:
    virtual std::unique_ptr<ASTConsumer>
    CreateASTConsumer(CompilerInstance &CI, StringRef InFile) override {
        OS = CI.createDefaultOutputFile(true, InFile, "bc");
        std::vector<std::unique_ptr<ASTConsumer>> C;
        C.push_back(std::unique_ptr<ASTConsumer>(new ExtractMacroConsumer(MM)));
        C.push_back(std::unique_ptr<ASTConsumer>(
            Delegate::CreateASTConsumer(CI, InFile)));
        return llvm::make_unique<MultiplexConsumer>(std::move(C));
    }

    virtual bool BeginInvocation(CompilerInstance &CI) override {
        return Delegate::BeginInvocation(CI);
    }

    virtual bool BeginSourceFileAction(CompilerInstance &CI) override {
        Delegate::setCurrentInput(super::getCurrentInput());
        Delegate::setCompilerInstance(&CI);
        return Delegate::BeginSourceFileAction(CI);
    }

    virtual void ExecuteAction() override { Delegate::ExecuteAction(); }

    virtual void EndSourceFileAction() override {
        Delegate::EndSourceFileAction();
        std::unique_ptr<llvm::Module> M(std::move(Delegate::takeModule()));
        if (!M)
            return;
        markMacroLocations(*M);
        WriteBitcodeToFile(*M, *OS);
        // why reset() is necessay?
        OS.reset();
    }

  private:
    MacroMap MM;
    std::unique_ptr<raw_ostream> OS;

    void markMacroLocations(llvm::Module &);
};

} // anonymous namespace

void IntAction::markMacroLocations(llvm::Module &M) {
    // Filename => set<Line>.
    LLVMContext &C = M.getContext();
    unsigned MD_macro = C.getMDKindID("macro");
    CompilerInstance &CI = super::getCompilerInstance();
    Preprocessor &PP = CI.getPreprocessor();
    for (Function &F : M) {
        for (auto &I : instructions(F)) {
            const DebugLoc &DbgLoc = I.getDebugLoc();
            if (!DbgLoc)
                continue;
            SmallVector<char, 4> Path;
            llvm::sys::path::append(Path, DbgLoc->getDirectory(),
                                    DbgLoc->getFilename());
            StringRef Filename(Path.begin(), Path.size());
            unsigned Line = DbgLoc->getLine();
            SmallVector<Metadata *, 4> MDElems;
            for (SourceLocation Loc : MM.lookup(Filename).lookup(Line)) {
                StringRef MacroName = PP.getImmediateMacroName(Loc);
                MDElems.push_back(MDString::get(C, MacroName));
            }
            if (MDElems.empty())
                continue;
            I.setMetadata(MD_macro, MDNode::get(C, MDElems));
        }
    }
}

static FrontendPluginRegistry::Add<IntAction> X("intfe", "Intercept Frontend");
