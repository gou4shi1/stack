#include "BugOnUndef.h"
#include <llvm/IR/CallSite.h>

#define DEBUG_TYPE "bugon-undef"

using namespace llvm;

namespace {

bool isDeadArg(Instruction *I, unsigned Idx) {
    CallSite CS(I);
    if (!CS)
        return false;
    Function *F = CS.getCalledFunction();
    if (!F || F->empty() || Idx >= F->arg_size())
        return false;
    Function::arg_iterator A = F->arg_begin();
    for (unsigned i = 0; i < Idx; ++i)
        ++A;
    return A->use_empty();
}

bool isDeadRet(Function *F) {
    for (const auto &U : F->uses()) {
        CallSite CS(U);
        if (!CS)
            return false;
        if (!CS->use_empty())
            return false;
    }
    return true;
}

} // anonymous namespace

bool BugOnUndefPass::runOnInstruction(Instruction *I) {
    // It's okay to have undef in phi's operands.
    // TODO: catch conditional undefs.
    if (isa<PHINode>(I) || isa<SelectInst>(I))
        return false;
    if (isa<InsertValueInst>(I) || isa<InsertElementInst>(I))
        return false;
    // Allow ret undef if the return value is never used.
    if (isa<ReturnInst>(I)) {
        if (isDeadRet(I->getParent()->getParent()))
            return false;
    }
    // If any operand is undef, this instruction must not be reachable.
    for (unsigned i = 0, n = I->getNumOperands(); i != n; ++i) {
        Value *V = I->getOperand(i);
        if (isa<UndefValue>(V)) {
            // Allow undef arguments created by -deadargelim,
            // which are basically unused in the function body.
            if (isDeadArg(I, i))
                continue;
            return insert(Builder->getTrue(), "undef");
        }
    }
    return false;
}
