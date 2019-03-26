#include "PathGen.h"
#include "ValueGen.h"
#include "SMTHelper.h"
#include <llvm/Support/SMTAPI.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Instructions.h>

using namespace llvm;

#define SMT VG.Solver

static BasicBlock *findCommonDominator(BasicBlock *BB, DominatorTree *DT) {
	BasicBlock *Dom = *pred_begin(BB);
    for (const auto &Pred: predecessors(BB)) {
		Dom = DT->findNearestCommonDominator(Dom, Pred);
    }
	return Dom;
}

SMTExprRef PathGen::get(BasicBlock *BB) {
	SMTExprRef G = Cache.lookup(BB);
	if (G)
		return G;
	// Entry block has true guard.
	if (BB == &BB->getParent()->getEntryBlock()) {
		G = mkBVTrue(SMT);
		Cache[BB] = G;
		return G;
	}
	if (DT) {
		// Fall back to common ancestors if any back edges.
        for (const auto &Pred: predecessors(BB)) {
            if (isBackedge(Pred, BB))
				return get(findCommonDominator(BB, DT));
        }
	}
	// The guard is the disjunction of predecessors' guards.
	// Initialize to false.
	G = mkBVFalse(SMT);
    for (const auto &Pred: predecessors(BB)) {
		// Skip back edges.
		if (!DT && isBackedge(Pred, BB))
			continue;
		SMTExprRef Term = getTermGuard(Pred->getTerminator(), BB);
		SMTExprRef PN = getPHIGuard(BB, Pred);
		SMTExprRef TermWithPN = SMT->mkBVAdd(Term, PN);
		SMTExprRef Br = SMT->mkBVAnd(TermWithPN, get(Pred));
		G = SMT->mkBVOr(G, Br);
	}
	Cache[BB] = G;
	return G;
}

bool PathGen::isBackedge(llvm::BasicBlock *From, llvm::BasicBlock *To) {
	return std::find(Backedges.begin(), Backedges.end(), Edge(From, To))
		!= Backedges.end();
}

SMTExprRef PathGen::getPHIGuard(BasicBlock *BB, BasicBlock *Pred) {
	SMTExprRef E = mkBVTrue(SMT);
    for (auto i = BB->begin(), e = BB->end(); i != e; ++i) {
		PHINode *I = dyn_cast<PHINode>(i);
		if (!I)
			break;
		Value *V = I->getIncomingValueForBlock(Pred);
		// Skip undef.
		if (isa<UndefValue>(V))
			continue;
		// Skip non-integral types.
		if (!ValueGen::isAnalyzable(V))
			continue;
		// Generate I == V.
		SMTExprRef PN = bool2bv(SMT, SMT->mkEqual(VG.get(I), VG.get(V)));
		E = SMT->mkBVAnd(E, PN);
	}
	return E;
}

SMTExprRef PathGen::getTermGuard(Instruction *I, BasicBlock *BB) {
	switch (I->getOpcode()) {
	default: I->dump(); llvm_unreachable("Unknown terminator!");
	case Instruction::Br:
		return getTermGuard(cast<BranchInst>(I), BB);
	case Instruction::Switch:
		return getTermGuard(cast<SwitchInst>(I), BB);
	case Instruction::IndirectBr:
	case Instruction::Invoke:
		return mkBVTrue(SMT);
	}
}

SMTExprRef PathGen::getTermGuard(BranchInst *I, BasicBlock *BB) {
	if (I->isUnconditional())
		return mkBVTrue(SMT);
	// true branch.
	SMTExprRef E = VG.get(I->getCondition());
	// false branch.
	if (I->getSuccessor(0) != BB) {
		assert(I->getSuccessor(1) == BB);
		SMTExprRef E = SMT->mkBVNot(E);
	}
	return E;
}

SMTExprRef PathGen::getTermGuard(SwitchInst *I, BasicBlock *BB) {
	SMTExprRef L = VG.get(I->getCondition());
	SwitchInst::CaseIt i = I->case_begin(), e = I->case_end();
	if (I->getDefaultDest() != BB) {
		// Find all x = C_i for BB.
		SMTExprRef E = mkBVFalse(SMT);
        for (const auto &Case: I->cases()) {
			if (Case.getCaseSuccessor() == BB) {
				ConstantInt *CI = Case.getCaseValue();
				SMTExprRef Cond = bool2bv(SMT, SMT->mkEqual(L, VG.get(CI)));
				E = SMT->mkBVOr(E, Cond);
			}
		}
		return E;
	}
	// Compute guard for the default case.
	// i starts from 1; 0 is reserved for the default.
	SMTExprRef E = mkBVFalse(SMT);
    for (const auto &Case: I->cases()) {
        ConstantInt *CI = Case.getCaseValue();
        SMTExprRef Cond = bool2bv(SMT, SMT->mkEqual(L, VG.get(CI)));
        E = SMT->mkBVOr(E, Cond);
    }
	return SMT->mkBVNot(E);
}
