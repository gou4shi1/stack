#include "CFGElim.h"
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Utils/Local.h>

#define DEBUG_TYPE "cfg-elim"

using namespace llvm;

PreservedAnalyses CFGElimPass::run(Function &F, FunctionAnalysisManager &FAM) {
    DominatorTree &DT = FAM.getResult<DominatorTreeAnalysis>(F);
    DomTreeUpdater DTU(DT, DomTreeUpdater::UpdateStrategy::Lazy);
    bool Changed = false;
    Changed |= removeUnreachableBlocks(F, nullptr, &DTU);
    DTU.flush();
    for (auto i = F.begin(), e = F.end(); i != e;) {
        BasicBlock *BB = &*i++;
        Changed |= ConstantFoldTerminator(BB, true, nullptr, &DTU);
        Changed |= EliminateDuplicatePHINodes(BB);
        // Must be the last one.
        Changed |= MergeBlockIntoPredecessor(BB, &DTU);
    }
    DTU.flush();
    if (!Changed)
        return PreservedAnalyses::all();
    PreservedAnalyses PA;
    PA.preserve<DominatorTreeAnalysis>();
    return PA;
}
