#include "NullCheckElim.h"
#include "DebugLocHelper.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>

#define DEBUG_TYPE "null-check-elim"

using namespace llvm;

PreservedAnalyses NullCheckElimPass::run(Function &F,
                                         FunctionAnalysisManager &FAM) {
    for (auto &BB : F)
        MarkNullCheckEmitByLLVM(BB);
    return PreservedAnalyses::all();
}

bool NullCheckElimPass::MarkNullCheckEmitByLLVM(BasicBlock &BB) {
    // Clang emits BB with this special name for BB.
    // It works better with overloaded new/delete.
    StringRef Name = BB.getName();
    if (!Name.startswith("new.notnull") && !Name.startswith("delete.notnull") &&
        !Name.startswith("cast.notnull"))
        return false;
    BasicBlock *Pred = BB.getSinglePredecessor();
    if (!Pred)
        return false;
    BranchInst *BI = dyn_cast<BranchInst>(Pred->getTerminator());
    if (!BI || !BI->isConditional())
        return false;
    // Remove debugging information to ignore the check.
    return clearDebugLoc(BI->getCondition());
}
