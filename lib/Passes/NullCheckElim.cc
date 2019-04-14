#include "NullCheckElim.h"
#include "DebugLocHelper.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/MemorySSA.h>
#include <llvm/Analysis/MemorySSAUpdater.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/Transforms/Utils/Local.h>

#define DEBUG_TYPE "null-check-elim"

using namespace llvm;

PreservedAnalyses NullCheckElimPass::run(Function &F,
                                         FunctionAnalysisManager &FAM) {
    DT = &FAM.getResult<DominatorTreeAnalysis>(F);
    LI = &FAM.getResult<LoopAnalysis>(F);
    MSSA = &FAM.getResult<MemorySSAAnalysis>(F).getMSSA();
    MemDep = &FAM.getResult<MemoryDependenceAnalysis>(F);
    TLI = &FAM.getResult<TargetLibraryAnalysis>(F);
    DomTreeUpdater DTU(*DT, DomTreeUpdater::UpdateStrategy::Eager);
    auto MSSAU = MemorySSAUpdater(MSSA);
    bool Changed = false;
    // TODO: split SimplifyCFG related into another file
    Changed |= removeUnreachableBB(F);
    for (Function::iterator i = F.begin(), e = F.end(); i != e;) {
        BasicBlock *BB = &*i++;
        Changed |= DeleteNullCheckBB(BB);
        Changed |= ConstantFoldTerminator(BB, true);
        Changed |= EliminateDuplicatePHINodes(BB);
        // Must be the last one.
        Changed |= MergeBlockIntoPredecessor(BB, &DTU, LI, &MSSAU, MemDep);
    }
    return Changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
}

bool NullCheckElimPass::removeUnreachableBB(Function &F) {
    SmallVector<BasicBlock *, 4> UnreachableBB;
    for (auto &BB : F) {
        if (DT->isReachableFromEntry(&BB))
            continue;
        for (auto Succ : successors(&BB)) {
            if (DT->isReachableFromEntry(Succ))
                Succ->removePredecessor(&BB);
        }
        BB.dropAllReferences();
        UnreachableBB.push_back(&BB);
    }
    if (UnreachableBB.empty())
        return false;
    for (BasicBlock *BB : UnreachableBB)
        BB->eraseFromParent();
    return true;
}

bool NullCheckElimPass::DeleteNullCheckBB(BasicBlock *BB) {
    // Clang emits BB with this special name for BB.
    // It works better with overloaded new/delete.
    StringRef Name = BB->getName();
    if (!Name.startswith("new.notnull") && !Name.startswith("delete.notnull") &&
        !Name.startswith("cast.notnull"))
        return false;
    BasicBlock *Pred = BB->getSinglePredecessor();
    if (!Pred)
        return false;
    BranchInst *BI = dyn_cast<BranchInst>(Pred->getTerminator());
    if (!BI || !BI->isConditional())
        return false;
    // Remove debugging information to ignore the check.
    return clearDebugLoc(BI->getCondition());
}
