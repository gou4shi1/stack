#include "BugOnFree.h"
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/Analysis/MemoryBuiltins.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/IR/CallSite.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/InstIterator.h>

#define DEBUG_TYPE "bugon-free"

using namespace llvm;

void BugOnFreePass::initialize(Function &F, FunctionAnalysisManager &FAM) {
    TLI = &FAM.getResult<TargetLibraryAnalysis>(F);
    DT = &FAM.getResult<DominatorTreeAnalysis>(F);
    FreePtrs.clear();
    for (auto &I : instructions(F)) {
        if (!I.getDebugLoc())
            continue;
        CallSite CS(&I);
        if (!CS || !CS.getCalledFunction())
            continue;
        if (Use *U = extractFree(CS))
            FreePtrs.insert(U);
    }
}

bool BugOnFreePass::runOnInstruction(Instruction *I) {
    if (FreePtrs.empty())
        return false;

    Value *P = getNonvolatileBaseAddress(I);
    if (!P)
        return false;

    bool Changed = false;
    // free(x): x == p (p must be nonnull).
    for (Use *U : FreePtrs) {
        Instruction *FreeCall = cast<Instruction>(U->getUser());
        if (!DT->dominates(FreeCall, I))
            continue;
        Value *X = U->get();
        Value *V = createPointerEQ(X, P);
        // x' = realloc(x, n): x == p && x' != null.
        if (FreeCall->getType()->isPointerTy())
            V = createAnd(createIsNotNull(FreeCall), V);
        StringRef Name = CallSite(FreeCall).getCalledFunction()->getName();
        Changed |= insert(V, Name, FreeCall->getDebugLoc());
    }
    return Changed;
}

Use *BugOnFreePass::extractFree(CallSite CS) {
#define P std::make_pair
    static std::pair<const char *, int> Frees[] = {
        P("kfree", 0),
        P("vfree", 0),
        P("__kfree_skb", 0),
    };
#undef P
    // Builtin free/delete/realloc.
    Instruction *I = CS.getInstruction();
    if (isFreeCall(I, TLI) || (isAllocationFn(I, TLI) && !isAllocLikeFn(I, TLI)))
        return CS.arg_begin();
    // Custom function.
    StringRef Name = CS.getCalledFunction()->getName();
    for (unsigned i = 0; i < sizeof(Frees) / sizeof(Frees[0]); i++) {
        if (Name == Frees[i].first)
            return CS.arg_begin() + Frees[i].second;
    }
    return NULL;
}
