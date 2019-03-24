#define DEBUG_TYPE "bugon-int"

#include "BugOn.h"

using namespace llvm;

namespace stack {

class BugOnIntPass : public BugOnPass {
	bool runOnInstruction(Instruction *) override;
	bool visitShiftOperator(IntegerType *, Value *R, const char *Bug);

public:
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) override;
};

PreservedAnalyses BugOnIntPass::run(Function &F, FunctionAnalysisManager &FAM) {
    bool changed = runOnInstructionsOfFunction(F);
    return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
}

bool BugOnIntPass::runOnInstruction(Instruction *I) {
	BinaryOperator *BO = dyn_cast<BinaryOperator>(I);
	if (!BO) return false;
	IntegerType *T = dyn_cast<IntegerType>(I->getType());
	if (!T) return false;
	Value *L = I->getOperand(0);
	Value *R = I->getOperand(1);
	bool Changed = false;
	switch (BO->getOpcode()) {
	default: break;
	case Instruction::Add:
		if (BO->hasNoSignedWrap())
			Changed |= insert(createIsSAddWrap(L, R), "signed addition overflow");
		if (BO->hasNoUnsignedWrap())
			Changed |= insert(createIsUAddWrap(L, R), "unsigned addition overflow");
		break;
	case Instruction::Sub:
		if (BO->hasNoSignedWrap())
			Changed |= insert(createIsSSubWrap(L, R), "signed subtraction overflow");
		if (BO->hasNoUnsignedWrap())
			Changed |= insert(createIsUSubWrap(L, R), "unsigned subtraction overflow");
		break;
	case Instruction::Mul:
		if (BO->hasNoSignedWrap())
			Changed |= insert(createIsSMulWrap(L, R), "signed multiplication overflow");
		if (BO->hasNoUnsignedWrap())
			Changed |= insert(createIsUMulWrap(L, R), "unsigned multiplication overflow");
		break;
	case Instruction::SDiv:
	case Instruction::SRem:
		Changed |= insert(createIsSDivWrap(L, R), "signed division overflow");
		// Fall through.
	case Instruction::UDiv:
	case Instruction::URem:
		Changed |= insert(createIsZero(R), "division by zero");
		break;
	case Instruction::Shl:
		Changed |= visitShiftOperator(T, R, "shift left overflow");
		if (BO->hasNoSignedWrap()) {
			Value *Power = Builder->CreateShl(ConstantInt::get(T, 1), R);
			Changed |= insert(createIsSMulWrap(L, Power), "signed shift left overflow");
		}
		if (BO->hasNoUnsignedWrap()) {
			Value *Power = Builder->CreateShl(ConstantInt::get(T, 1), R);
			Changed |= insert(createIsUMulWrap(L, Power), "unsigned shift left overflow");
		}
		break;
	case Instruction::LShr:
		Changed |= visitShiftOperator(T, R, "logical shift right overflow");
		break;
	case Instruction::AShr:
		Changed |= visitShiftOperator(T, R, "arithmetic shift right overflow");
		break;
	}
	return Changed;
}

bool BugOnIntPass::visitShiftOperator(IntegerType *T, Value *R, const char *Bug) {
	Constant *BitWidth = ConstantInt::get(T, T->getBitWidth());
	Value *V = Builder->CreateICmpUGE(R, BitWidth);
	return insert(V, Bug);
}

} // end stack namespace
