#pragma once

#include "llvm/IR/PassManager.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Dominators.h"

namespace stack {

llvm::Function *getBugOn(const llvm::Module *);
llvm::Function *getOrInsertBugOn(llvm::Module *);

class BugOnInst : public llvm::CallInst {
public:
	typedef llvm::CallInst CallInst;
	typedef llvm::Function Function;
	typedef llvm::StringRef StringRef;
	typedef llvm::Value Value;

	Value *getCondition() const { return getArgOperand(0); }
	StringRef getAnnotation() const;

	// For LLVM casts.
	static bool classof(const CallInst *I) {
		if (const Function *F = I->getCalledFunction())
			return getBugOn(F->getParent()) == F;
		return false;
	}
	static bool classof(const Value *V) {
		return llvm::isa<CallInst>(V) && classof(llvm::cast<CallInst>(V));
	}
};

class BugOnPass : public llvm::PassInfoMixin<BugOnPass> {
public:
    typedef llvm::PreservedAnalyses PreservedAnalyses;
    typedef llvm::FunctionAnalysisManager FunctionAnalysisManager;
	typedef llvm::DataLayout DataLayout;
	typedef llvm::DominatorTree DominatorTree;
	typedef llvm::IRBuilder<> BuilderTy;
	typedef llvm::Value Value;
	typedef llvm::Function Function;
	typedef llvm::BasicBlock BasicBlock;
	typedef llvm::Instruction Instruction;

private:
	Function *BugOn;
	unsigned int MD_bug;
    BasicBlock *InsertBB;
    BasicBlock::iterator InsertPt;

public:
    virtual PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) = 0;

	static bool clearDebugLoc(Value *);
	static bool recursivelyClearDebugLoc(Value *);

	static Value *getUnderlyingObject(Value *, const DataLayout &);
	static Value *getAddressOperand(Value *, bool skipVolatile = false);
	static Value *getNonvolatileAddressOperand(Value *V) {
		return getAddressOperand(V, true);
	}
	static Value *getNonvolatileBaseAddress(Value *V, const DataLayout &DL) {
		if (Value *P = getNonvolatileAddressOperand(V))
			return getUnderlyingObject(P, DL);
		return NULL;
	}

protected:
	typedef BugOnPass super;

	BuilderTy *Builder;

	virtual bool runOnInstruction(Instruction *) = 0;
    bool runOnInstructionsOfFunction(Function &);

	bool insert(Value *, llvm::StringRef Bug);
	bool insert(Value *, llvm::StringRef Bug, const llvm::DebugLoc &);
	llvm::Module *getModule();
    void backupInsertPoint();
    void restoreInsertPoint();
    void setInsertPoint(Instruction *);
    void setInsertPointAfter(Instruction *);

	Value *createIsNull(Value *, const DataLayout &, const Instruction *, const DominatorTree *);
	Value *createIsNotNull(Value *, const DataLayout &, const Instruction *, const DominatorTree *);
	Value *createIsZero(Value *);
	Value *createIsWrap(llvm::Intrinsic::ID, Value *, Value *);
	Value *createIsSAddWrap(Value *, Value *);
	Value *createIsUAddWrap(Value *, Value *);
	Value *createIsSSubWrap(Value *, Value *);
	Value *createIsUSubWrap(Value *, Value *);
	Value *createIsSMulWrap(Value *, Value *);
	Value *createIsUMulWrap(Value *, Value *);
	Value *createIsSDivWrap(Value *, Value *);
	Value *createAnd(Value *, Value *);
	Value *createSExtOrTrunc(Value *, llvm::IntegerType *);
	Value *createPointerEQ(Value *, Value *);
};

} // end stack namespace
