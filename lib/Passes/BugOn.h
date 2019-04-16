#pragma once

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/PassManager.h>

llvm::Function *getBugOn(const llvm::Module *);
llvm::Function *getOrInsertBugOn(llvm::Module *);

class BugOnInst : public llvm::CallInst {
  public:
    llvm::Value *getCondition() const { return getArgOperand(0); }
    llvm::StringRef getAnnotation() const;

    // For LLVM casts.
    static bool classof(const llvm::CallInst *I) {
        if (const llvm::Function *F = I->getCalledFunction())
            return getBugOn(F->getParent()) == F;
        return false;
    }
    static bool classof(const llvm::Value *V) {
        return llvm::isa<llvm::CallInst>(V) &&
               classof(llvm::cast<llvm::CallInst>(V));
    }
};

class BugOnPass {
    llvm::Function *BugOn;
    unsigned int MD_bug;
    llvm::BasicBlock *InsertBB;
    llvm::BasicBlock::iterator InsertPt;

    llvm::Value *getUnderlyingObject(llvm::Value *);
    llvm::Value *getAddressOperand(llvm::Value *, bool skipVolatile = false);

  public:
    virtual llvm::PreservedAnalyses run(llvm::Function &,
                                        llvm::FunctionAnalysisManager &);

    llvm::Value *getNonvolatileAddressOperand(llvm::Value *V) {
        return getAddressOperand(V, true);
    }
    llvm::Value *getNonvolatileBaseAddress(llvm::Value *V) {
        if (llvm::Value *P = getNonvolatileAddressOperand(V))
            return getUnderlyingObject(P);
        return nullptr;
    }

  protected:
    using super = BugOnPass;
    using BuilderTy = llvm::IRBuilder<>;

    BuilderTy *Builder;
    const llvm::DataLayout *DL;

    virtual void getAnalysis(llvm::Function &F, llvm::FunctionAnalysisManager &FAM) {};
    virtual bool runOnInstruction(llvm::Instruction *) = 0;
    bool runOnInstructionsOfFunction(llvm::Function &);

    bool insert(llvm::Value *, llvm::StringRef Bug);
    bool insert(llvm::Value *, llvm::StringRef Bug, const llvm::DebugLoc &);
    llvm::Module *getInsertModule();
    void backupInsertPoint();
    void restoreInsertPoint();
    void setInsertPoint(llvm::Instruction *);
    void setInsertPointAfter(llvm::Instruction *);

    llvm::Value *createIsNull(llvm::Value *);
    llvm::Value *createIsNotNull(llvm::Value *);
    llvm::Value *createIsZero(llvm::Value *);
    llvm::Value *createIsWrap(llvm::Intrinsic::ID, llvm::Value *,
                              llvm::Value *);
    llvm::Value *createIsSAddWrap(llvm::Value *, llvm::Value *);
    llvm::Value *createIsUAddWrap(llvm::Value *, llvm::Value *);
    llvm::Value *createIsSSubWrap(llvm::Value *, llvm::Value *);
    llvm::Value *createIsUSubWrap(llvm::Value *, llvm::Value *);
    llvm::Value *createIsSMulWrap(llvm::Value *, llvm::Value *);
    llvm::Value *createIsUMulWrap(llvm::Value *, llvm::Value *);
    llvm::Value *createIsSDivWrap(llvm::Value *, llvm::Value *);
    llvm::Value *createAnd(llvm::Value *, llvm::Value *);
    llvm::Value *createSExtOrTrunc(llvm::Value *, llvm::IntegerType *);
    llvm::Value *createPointerEQ(llvm::Value *, llvm::Value *);
};
