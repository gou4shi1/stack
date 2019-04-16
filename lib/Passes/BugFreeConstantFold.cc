#include "BugFreeConstantFold.h"
#include "DebugLocHelper.h"
#include "SMTHelper.h"
#include <llvm/IR/Constants.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/SMTAPI.h>
#include <llvm/Transforms/Utils/Local.h>

#define DEBUG_TYPE "bugfree-constant-fold"

using namespace llvm;

PreservedAnalyses
BugFreeConstantFoldPass::runOnFunction(Function &F,
                                       FunctionAnalysisManager &FAM) {
    bool Changed = false;
    for (auto i = inst_begin(F), e = inst_end(F); i != e;) {
        Instruction *I = &*i++;
        if (!hasSingleDebugLocation(I))
            continue;
        // For now we are only interested in bool expressions.
        if (!isa<ICmpInst>(I))
            continue;
        Optional<int> ConstVal = foldConst(I);
        // BENCHMARK(Diagnostic() << "query: " << qstr(ConstVal) << "\n");
        if (!ConstVal.hasValue())
            continue;
        ReportAndReplace(I, ConstVal.getValue());
        Changed = true;
    }
    if (!Changed)
        return PreservedAnalyses::all();
    PreservedAnalyses PA;
    PA.preserveSet<CFGAnalyses>();
    return PA;
}

Optional<int> BugFreeConstantFoldPass::foldConst(Instruction *I) {
    Optional<int> Result;
    BasicBlock *BB = I->getParent();
    SMTExprRef Delta = getBugFreeDelta(BB);
    if (!Delta)
        return Result;
    // Compute path condition.
    Solver->push();
    addBVConstraint(Solver, PG->get(BB));
    SMTExprRef E = VG->get(I);
    Optional<bool> sat = queryWithBugFreeDelta(E, Delta);
    if (sat.hasValue() && !sat.getValue()) {
        // I must be false with Delta.
        // Can I be true without Delta?
        sat = queryBV(Solver, E);
        if (sat.hasValue() && sat.getValue())
            Result = 0;
    } else {
        // I can be false with Delta.
        // Let's try if it can be true.
        E = Solver->mkBVNot(E);
        sat = queryWithBugFreeDelta(E, Delta);
        if (sat.hasValue() && !sat.getValue()) {
            // I must be true with Delta.
            // Can I be false with Delta?
            sat = queryBV(Solver, E);
            if (sat.hasValue() && sat.getValue())
                Result = 1;
        }
    }
    Solver->pop();
    return Result;
}

void BugFreeConstantFoldPass::ReportAndReplace(Instruction *I, int ConstVal) {
    Diag.bug(DEBUG_TYPE);
    Diag << "model: |\n"
         << *I << "\n  -->  " << (ConstVal ? "true" : "false") << "\n";
    Diag.backtrace(I);
    printMinimalAssertions();
    Constant *C = ConstantInt::get(I->getType(), ConstVal);
    I->replaceAllUsesWith(C);
    RecursivelyDeleteTriviallyDeadInstructions(I);
}
