#include "LoadElim.h"
#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/MemoryDependenceAnalysis.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Transforms/Utils/Local.h>

#define DEBUG_TYPE "load-elim"

using namespace llvm;

PreservedAnalyses LoadElimPass::run(Function &F, FunctionAnalysisManager &FAM) {
    AA = &FAM.getResult<AAManager>(F);
    MemDep = &FAM.getResult<MemoryDependenceAnalysis>(F);
    TLI = &FAM.getResult<TargetLibraryAnalysis>(F);
    bool Changed = false;
    for (auto i = inst_begin(F), e = inst_end(F); i != e;) {
        if (LoadInst *LI = dyn_cast<LoadInst>(&*i++))
            Changed |= merge(LI);
    }
    return Changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
}

// For now just merge loads in the same block.
bool LoadElimPass::merge(LoadInst *I) {
    if (I->isVolatile())
        return false;
    Instruction *Dep = MemDep->getDependency(I).getInst();
    if (!Dep)
        return false;
    Value *P = NULL, *V = NULL;
    // Find a previous load/store.
    if (LoadInst *LI = dyn_cast<LoadInst>(Dep)) {
        P = LI->getPointerOperand();
        V = LI;
    } else if (StoreInst *SI = dyn_cast<StoreInst>(Dep)) {
        P = SI->getPointerOperand();
        V = SI->getValueOperand();
    }
    if (!P || !V)
        return false;
    // Must be the same type.
    if (V->getType() != I->getType())
        return false;
    // Must be the same address.
    if (!AA->isMustAlias(P, I->getPointerOperand()))
        return false;
    I->replaceAllUsesWith(V);
    RecursivelyDeleteTriviallyDeadInstructions(I, TLI);
    return true;
}
