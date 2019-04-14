#include "BugFreeAlgebraSimply.h"
#include "DebugLocHelper.h"
#include "SMTHelper.h"
#include <llvm/Analysis/AliasAnalysis.h>
#include <llvm/Analysis/ScalarEvolutionExpander.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/Support/SMTAPI.h>
#include <llvm/Transforms/Utils/Local.h>

#define DEBUG_TYPE "bugfree-algebra-simply"

using namespace llvm;

namespace {

void getAdditiveTerms(const SCEV *S, SmallVectorImpl<const SCEV *> &Terms) {
    const SCEVAddExpr *A = dyn_cast<SCEVAddExpr>(S);
    if (!A) {
        Terms.push_back(S);
        return;
    }
    Terms.append(A->op_begin(), A->op_end());
    std::sort(Terms.begin(), Terms.end());
}

bool contains(const SCEV *L, const SCEV *R) {
    SmallVector<const SCEV *, 4> LTerms, RTerms;
    getAdditiveTerms(L, LTerms);
    getAdditiveTerms(R, RTerms);
    if (LTerms.size() < RTerms.size())
        LTerms.swap(RTerms);
    // Ignore LHS cmp C for now.
    if (RTerms.size() == 1) {
        if (isa<SCEVConstant>(RTerms[0]))
            return false;
    }
    return std::includes(LTerms.begin(), LTerms.end(), RTerms.begin(),
                         RTerms.end());
}

const char *getPredicateStr(CmpInst::Predicate Pred) {
    switch (Pred) {
    default:
        assert(0);
    case CmpInst::ICMP_EQ:
        return " == ";
    case CmpInst::ICMP_NE:
        return " != ";
    case CmpInst::ICMP_UGT:
        return " >u ";
    case CmpInst::ICMP_UGE:
        return " ≥u ";
    case CmpInst::ICMP_ULT:
        return " <u ";
    case CmpInst::ICMP_ULE:
        return " ≤u ";
    case CmpInst::ICMP_SGT:
        return " >s ";
    case CmpInst::ICMP_SGE:
        return " ≥s ";
    case CmpInst::ICMP_SLT:
        return " <s ";
    case CmpInst::ICMP_SLE:
        return " ≤s ";
    }
}
/*
inline const char *qstr(int isEqv) {
        switch (isEqv) {
        default: return "timeout";
        case 0:  return "fail";
        case 1:  return "succ";
        }
}
*/
} // anonymous namespace

bool BugFreeAlgebraSimplyPass::runOnFunction(Function &F,
                                             FunctionAnalysisManager &FAM) {
    SE = &FAM.getResult<ScalarEvolutionAnalysis>(F);
    TLI = &FAM.getResult<TargetLibraryAnalysis>(F);
    bool Changed = false;
    for (auto i = inst_begin(F), e = inst_end(F); i != e;) {
        Instruction *I = &*i++;
        if (!hasSingleDebugLocation(I))
            continue;
        // For now we are only interested in comparisons.
        if (ICmpInst *ICI = dyn_cast<ICmpInst>(I))
            Changed |= visitICmpInst(ICI);
    }
    return Changed;
}

bool BugFreeAlgebraSimplyPass::visitICmpInst(ICmpInst *I) {
    const SCEV *L = SE->getSCEV(I->getOperand(0));
    const SCEV *R = SE->getSCEV(I->getOperand(1));
    // Is L part of R (or vice versa)?
    if (!contains(L, R))
        return false;
    const SCEV *S = SE->getMinusSCEV(L, R);
    LLVMContext &C = I->getContext();
    IntegerType *T = IntegerType::get(C, DL->getTypeSizeInBits(S->getType()));
    Value *V = SCEVExpander(*SE, *DL, "").expandCodeFor(S, T, I);
    Value *Z = Constant::getNullValue(T);
    // Transform (lhs op rhs) to ((lhs - rhs) op 0).
    ICmpInst *NewCmp = new ICmpInst(I, I->getSignedPredicate(), V, Z);
    NewCmp->setDebugLoc(I->getDebugLoc());
    int isEqv = checkEqv(I, NewCmp);
    // BENCHMARK(Diagnostic() << "query: " << qstr(isEqv) << "\n");
    if (!isEqv) {
        RecursivelyDeleteTriviallyDeadInstructions(NewCmp, TLI);
        return false;
    }
    Diag.bug(DEBUG_TYPE);
    Diag << "model: |\n"
         << *I << "\n  -->" << *NewCmp << "\n"
         << "  ************************************************************\n  "
         << *L << getPredicateStr(I->getPredicate()) << *R << "\n  -->  " << *S
         << getPredicateStr(I->getSignedPredicate()) << "0\n";
    Diag.backtrace(I);
    printMinimalAssertions();
    I->replaceAllUsesWith(NewCmp);
    RecursivelyDeleteTriviallyDeadInstructions(I, TLI);
    return true;
}

bool BugFreeAlgebraSimplyPass::checkEqv(ICmpInst *I0, ICmpInst *I1) {
    bool isEqv = false;
    Solver->push();
    BasicBlock *BB = I0->getParent();
    addBVConstraint(Solver, PG->get(BB));
    SMTExprRef Q = bool2bv(
        Solver, Solver->mkNot(Solver->mkEqual(VG->get(I0), VG->get(I1))));
    // E0 != E1 without bug-free assertions; must be reachable as well.
    Optional<bool> sat = queryBV(Solver, Q);
    if (sat.hasValue() && sat.getValue()) {
        SMTExprRef Delta = getBugFreeDelta(BB);
        if (Delta) {
            // E0 == E1 with bug-free assertions.
            Optional<bool> sat = queryWithBugFreeDelta(Q, Delta);
            if (sat.hasValue() && !sat.getValue()) {
                isEqv = true;
            }
        }
    }
    Solver->pop();
    return isEqv;
}
