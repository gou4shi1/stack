#include "BugOnBounds.h"
#include <llvm/Analysis/TargetLibraryInfo.h>

using namespace llvm;

void BugOnBoundsPass::initialize(Function &F, FunctionAnalysisManager &FAM) {
    TLI = &FAM.getResult<TargetLibraryAnalysis>(F);
    ObjSizeEval =
        llvm::make_unique<ObjectSizeOffsetEvaluator>(*DL, TLI, F.getContext());
}

bool BugOnBoundsPass::runOnInstruction(Instruction *I) {
    Value *Ptr = getNonvolatileAddressOperand(I);
    if (!Ptr)
        return false;
    SizeOffsetEvalType SizeOffset = ObjSizeEval->compute(Ptr);
    if (!ObjSizeEval->bothKnown(SizeOffset))
        return false;
    Value *Size = SizeOffset.first;
    Value *Offset = SizeOffset.second;
    Type *T = Offset->getType();
    assert(T == Size->getType());
    Type *ElemTy = cast<PointerType>(Ptr->getType())->getElementType();
    Value *StoreSize = ConstantInt::get(T, DL->getTypeStoreSize(ElemTy));
    // Bug condition: Offset < 0.
    {
        Value *V = Builder->CreateICmpSLT(Offset, Constant::getNullValue(T));
        insert(V, "buffer overflow");
    }
    // Bug condition: Offset > Size.
    {
        Value *V = Builder->CreateICmpUGT(Offset, Size);
        insert(V, "buffer overflow");
    }
    // Bug condition: Size - Offset < StoreSize.
    {
        Value *V =
            Builder->CreateICmpULT(Builder->CreateSub(Size, Offset), StoreSize);
        insert(V, "buffer overflow");
    }
    return true;
}
