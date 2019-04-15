#include "CFGElim.h"
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Utils/Local.h>

#define DEBUG_TYPE "cfg-elim"

using namespace llvm;

PreservedAnalyses CFGElimPass::run(Function &F,
                                         FunctionAnalysisManager &FAM) {
    DT = &FAM.getResult<DominatorTreeAnalysis>(F);
    bool Changed = false;
    Changed |= removeUnreachableBB(F);
    DomTreeUpdater DTU(*DT, DomTreeUpdater::UpdateStrategy::Lazy);
    for (auto i = F.begin(), e = F.end(); i != e;) {
        BasicBlock *BB = &*i++;
        Changed |= ConstantFoldTerminator(BB, true, nullptr, &DTU);
        Changed |= EliminateDuplicatePHINodes(BB);
        // Must be the last one.
        Changed |= MergeBlockIntoPredecessor(BB, &DTU);
    }
    if (!Changed)
        return PreservedAnalyses::all();
    PreservedAnalyses PA;
    PA.preserve<DominatorTreeAnalysis>();
    return PA;
}

bool CFGElimPass::removeUnreachableBB(Function &F) {
    bool Changed = false;
    DomTreeUpdater DTU(*DT, DomTreeUpdater::UpdateStrategy::Lazy);
    for (auto &BB : F) {
        if (DT->isReachableFromEntry(&BB))
            continue;
        DTU.deleteBB(&BB);
        Changed = true;
    }
    return Changed;
}
