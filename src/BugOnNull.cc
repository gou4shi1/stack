#define DEBUG_TYPE "bugon-null"
#include "BugOn.h"
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/Analysis/ValueTracking.h>
#include <llvm/IR/DataLayout.h>

using namespace llvm;

namespace {

struct BugOnNull : BugOnPass {
	static char ID;
	BugOnNull() : BugOnPass(ID) {
		PassRegistry &Registry = *PassRegistry::getPassRegistry();
		initializeDataLayoutPass(Registry);
	}

	virtual void getAnalysisUsage(AnalysisUsage &AU) const {
		super::getAnalysisUsage(AU);
		AU.addRequired<DataLayout>();
	}

	virtual bool runOnFunction(Function &);
	virtual bool runOnInstruction(Instruction *);

private:
	DataLayout *DL;
	SmallPtrSet<Value *, 32> Visited;
};

} // anonymous namespace

bool BugOnNull::runOnFunction(Function &F) {
	DL = &getAnalysis<DataLayout>();
	return super::runOnFunction(F);
}

bool BugOnNull::runOnInstruction(Instruction *I) {
	Value *P = NULL;
	if (isa<TerminatorInst>(I)) {
		Visited.clear();
	} else if (LoadInst *LI = dyn_cast<LoadInst>(I)) {
		if (!LI->isVolatile())
			P = LI->getPointerOperand();
	} else if (StoreInst *SI = dyn_cast<StoreInst>(I)) {
		if (!SI->isVolatile())
			P = SI->getPointerOperand();
	}
	if (!P)
		return false;
	// Strip pointer offset to get the base pointer.
	Value *Base = GetUnderlyingObject(P, DL, 1000);
	if (!Visited.insert(Base))
		return false;
	return insert(createIsNull(Base), "null pointer dereference");
}

char BugOnNull::ID;

static RegisterPass<BugOnNull>
X("bugon-null", "Insert bugon calls for possible null pointer dereference");
