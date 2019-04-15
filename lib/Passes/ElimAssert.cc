#include "ElimAssert.h"
#include "Diagnostic.h"
#include <llvm/IR/CFG.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InstIterator.h>
#include <set>

#define DEBUG_TYPE "elim-assert"

using namespace llvm;

namespace {

std::set<StringRef> AssertFailures = {
    "__assert_fail", // Linux assert()
    "panic",         // Linux kernel BUG_ON()
};

std::set<StringRef> SafeFunctions = {
    "printf",
    "printk", // Linux kernel
};

} // anonymous namespace

PreservedAnalyses ElimAssertPass::run(Function &F,
                                      FunctionAnalysisManager &FAM) {
    for (auto i = inst_begin(F), e = inst_end(F); i != e;) {
        Instruction *I = &*i++;
        if (!I->getDebugLoc())
            continue;
        CallInst *CI = dyn_cast<CallInst>(I);
        if (!CI || !CI->getCalledFunction())
            continue;
        if (AssertFailures.find(CI->getCalledFunction()->getName()) ==
            AssertFailures.end())
            continue;
        BasicBlock *BB = CI->getParent();
        if (!safeBB(BB))
            continue;
        dropBB(F, BB);
    }
    return PreservedAnalyses::all();
}

// Return true if this basic block has no calls to functions that
// might prevent an eventual assertion failure.
bool ElimAssertPass::safeBB(BasicBlock *BB) {
    for (auto &I : *BB) {
        CallInst *CI = dyn_cast<CallInst>(&I);
        if (!CI)
            continue;
        if (!CI->getCalledFunction())
            return false;
        StringRef Name = CI->getCalledFunction()->getName();
        if (AssertFailures.find(Name) == AssertFailures.end() &&
            SafeFunctions.find(Name) == SafeFunctions.end()) {
            // Diagnostic() << "ElimAssert: unsafe call to " << N << "\n";
            return false;
        }
    }
    return true;
}

void ElimAssertPass::dropBB(Function &F, BasicBlock *BB) {
    IRBuilder<> TheBuilder(F.getContext());
    for (auto Pred : predecessors(BB)) {
        BranchInst *BI = dyn_cast<BranchInst>(Pred->getTerminator());
        if (!BI || BI->isUnconditional() || BI->getNumSuccessors() != 2)
            continue;
        if (BI->getSuccessor(0) == BB) {
            BI->setCondition(TheBuilder.getFalse());
        }
        if (BI->getSuccessor(1) == BB) {
            BI->setCondition(TheBuilder.getTrue());
        }
    }
}
