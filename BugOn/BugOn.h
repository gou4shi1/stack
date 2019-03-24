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
protected:
    typedef llvm::PreservedAnalyses PreservedAnalyses;
    typedef llvm::FunctionAnalysisManager FunctionAnalysisManager;
	typedef llvm::DataLayout DataLayout;
	typedef llvm::DominatorTree DominatorTree;
	typedef llvm::IRBuilder<> BuilderTy;
	typedef llvm::Value Value;
	typedef llvm::Module Module;
	typedef llvm::Function Function;
	typedef llvm::BasicBlock BasicBlock;
	typedef llvm::Instruction Instruction;
	typedef llvm::IntegerType IntegerType;
	typedef llvm::StringRef StringRef;
	typedef llvm::DebugLoc DebugLoc;

private:
	Function *BugOn;
	unsigned int MD_bug;
    BasicBlock *InsertBB;
    BasicBlock::iterator InsertPt;

	Value *getUnderlyingObject(Value *);
	Value *getAddressOperand(Value *, bool skipVolatile = false);
	Value *getNonvolatileAddressOperand(Value *V) {
		return getAddressOperand(V, true);
	}

public:
    virtual PreservedAnalyses run(Function &, FunctionAnalysisManager &);

	static bool clearDebugLoc(Value *);
	static bool recursivelyClearDebugLoc(Value *);

	Value *getNonvolatileBaseAddress(Value *V) {
		if (Value *P = getNonvolatileAddressOperand(V))
			return getUnderlyingObject(P);
		return NULL;
	}

protected:
	typedef BugOnPass super;

	BuilderTy *Builder;
	const DataLayout *DL;

	virtual bool runOnInstruction(Instruction *) = 0;
    bool runOnInstructionsOfFunction(Function &);

	bool insert(Value *, StringRef Bug);
	bool insert(Value *, StringRef Bug, const DebugLoc &);
	Module *getInsertModule();
    void backupInsertPoint();
    void restoreInsertPoint();
    void setInsertPoint(Instruction *);
    void setInsertPointAfter(Instruction *);

	Value *createIsNull(Value *);
	Value *createIsNotNull(Value *);
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
	Value *createSExtOrTrunc(Value *, IntegerType *);
	Value *createPointerEQ(Value *, Value *);
};

} // end stack namespace
