#include "BugFreeDCE.h"
#include "DebugLocHelper.h"
#include "SMTHelper.h"
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/SMTAPI.h>
#include <llvm/Transforms/Utils/Local.h>

#define DEBUG_TYPE "bugfree-dce"

using namespace llvm;

PreservedAnalyses BugFreeDCEPass::runOnFunction(Function &F,
                                                FunctionAnalysisManager &FAM) {
    bool Changed = false;
    DomTreeUpdater DTU(*DT, *PDT, DomTreeUpdater::UpdateStrategy::Eager);
    for (auto i = F.begin(), e = F.end(); i != e;) {
        BasicBlock *BB = &*i++;
        if (!shouldCheck(BB, &DTU, Changed))
            continue;
        // BENCHMARK(Diagnostic() << "query: " << qstr(Keep) << "\n");
        Changed |= check(BB, &DTU);
    }
    if (!Changed)
        return PreservedAnalyses::all();
    PreservedAnalyses PA;
    PA.preserve<DominatorTreeAnalysis>();
    PA.preserve<PostDominatorTreeAnalysis>();
    return PA;
}

bool BugFreeDCEPass::shouldCheck(BasicBlock *BB, DomTreeUpdater *DTU,
                                 bool &Changed) {
    // Ignore unreachable blocks, often from BUG_ON() or assert().
    if (isa<UnreachableInst>(BB->getTerminator()))
        return false;
    if (BB->getFirstInsertionPt()->isTerminator())
        return false;
    // BB may become unreachable after marking some block as unreachable.
    if (!DT->isReachableFromEntry(BB)) {
        markAsDead(BB, DTU);
        Changed = true;
        return false;
    }
    for (const auto &I : *BB) {
        if (hasSingleDebugLocation(&I))
            return true;
    }
    return false;
}

bool BugFreeDCEPass::check(BasicBlock *BB, DomTreeUpdater *DTU) {
    Function *F = BB->getParent();
    SMTExprRef R = PG->get(BB);
    Optional<bool> sat = queryBV(Solver, R);
    // Dead without BugFreeDelta.
    if (sat.hasValue() && !sat.getValue()) {
        markAsDead(BB, DTU);
        recalculate(*F);
        return true;
    }
    SMTExprRef Delta = getBugFreeDelta(BB);
    if (!Delta)
        return false;
    sat = queryWithBugFreeDelta(R, Delta);
    // Dead with BugFreeDelta.
    if (sat.hasValue() && !sat.getValue()) {
        ReportAndReplace(BB);
        markAsDead(BB, DTU);
        recalculate(*F);
        return true;
    }
    return false;
}

void BugFreeDCEPass::ReportAndReplace(BasicBlock *BB) {
    Diag.bug(DEBUG_TYPE);
    Diag << "model: |\n";
    for (auto Pred : predecessors(BB)) {
        auto BI = dyn_cast<BranchInst>(Pred->getTerminator());
        if (!BI || !BI->isConditional())
            continue;
        auto CondInst = dyn_cast<Instruction>(BI->getCondition());
        if (!CondInst)
            continue;
        bool CondVal = (BI->getSuccessor(1) == BB);
        Diag
            << *CondInst << "\n  -->  " << (CondVal ? "true" : "false")
            << "\n  "
               "************************************************************\n";
        BI->setCondition(ConstantInt::get(CondInst->getType(), CondVal));
        RecursivelyDeleteTriviallyDeadInstructions(CondInst);
    }
    Diag << "  " << BB->getName() << ":\n";
    for (const auto &I : *BB)
        Diag << I << '\n';
    for (const auto &I : *BB) {
        if (I.getDebugLoc()) {
            Diag.backtrace(&I);
            break;
        }
    }
    printMinimalAssertions();
}

void BugFreeDCEPass::markAsDead(BasicBlock *BB, DomTreeUpdater *DTU) {
    // Remove BB from successors and collect updates of DTU.
    std::vector<DominatorTree::UpdateType> Updates;
    for (auto *Succ : successors(BB)) {
        Succ->removePredecessor(BB);
        Updates.push_back({DominatorTree::Delete, BB, Succ});
    }
    // Empty BB; ignore PHI and LandingPad, which are trikcy to remove.
    for (auto i = BB->getFirstInsertionPt(), e = BB->end(); i != e;) {
        Instruction *I = &*i++;
        if (!I->use_empty())
            I->replaceAllUsesWith(UndefValue::get(I->getType()));
        I->eraseFromParent();
    }
    // Mark it as unreachable.
    new UnreachableInst(BB->getContext(), BB);
    // Apply permissive updates to DTU.
    DTU->applyUpdatesPermissive(Updates);
}
