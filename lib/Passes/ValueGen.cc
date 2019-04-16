#include "ValueGen.h"
#include "SMTHelper.h"
#include <llvm/ADT/APInt.h>
#include <llvm/IR/GetElementPtrTypeIterator.h>
#include <llvm/IR/InstVisitor.h>
#include <llvm/Support/SMTAPI.h>

using namespace llvm;

class ValueVisitor : public InstVisitor<ValueVisitor, SMTExprRef> {
    ValueGen *VG;

#define Solver VG->Solver
#define DL VG->DL

  public:
    SMTExprRef analyze(ValueGen *TheVG, Value *V) {
        VG = TheVG;

        if (!ValueGen::isAnalyzable(V)) {
            V->dump();
            assert(0 && "Unknown type!");
        }
        if (Instruction *I = dyn_cast<Instruction>(V))
            return visit(I);
        else if (Constant *C = dyn_cast<Constant>(V))
            return visitConstant(C);
        return mkBVFresh(V);
    }

    SMTExprRef visitConstant(Constant *C) {
        if (ConstantInt *CI = dyn_cast<ConstantInt>(C)) {
            const APInt &Int = CI->getValue();
            return mkBV(Solver, Int);
        }
        if (isa<ConstantPointerNull>(C)) {
            unsigned width = getBitWidth(C);
            return mkBV(Solver, APInt::getNullValue(width));
        }
        if (GEPOperator *GEP = dyn_cast<GEPOperator>(C))
            return visitGEPOperator(*GEP);
        return mkBVFresh(C);
    }

    SMTExprRef visitInstruction(Instruction &I) {
        SMTExprRef E = mkBVFresh(&I);
        // Ranges are constants, so don't worry about recursion.
        if (MDNode *MD = I.getMetadata("intrange"))
            addRangeConstraints(E, MD);
        return E;
    }

    SMTExprRef visitTruncInst(TruncInst &I) {
        unsigned DstWidth = getBitWidth(I.getDestTy());
        return Solver->mkBVExtract(DstWidth - 1, 0, get(I.getOperand(0)));
    }

    SMTExprRef visitZExtInst(ZExtInst &I) {
        unsigned DstWidth = getBitWidth(I.getDestTy());
        unsigned SrcWidth = getBitWidth(I.getSrcTy());
        return Solver->mkBVZeroExt(DstWidth - SrcWidth, get(I.getOperand(0)));
    }

    SMTExprRef visitSExtInst(SExtInst &I) {
        unsigned DstWidth = getBitWidth(I.getDestTy());
        unsigned SrcWidth = getBitWidth(I.getSrcTy());
        return Solver->mkBVSignExt(DstWidth - SrcWidth, get(I.getOperand(0)));
    }

    SMTExprRef visitBinaryOperator(BinaryOperator &I) {
        SMTExprRef L = get(I.getOperand(0)), R = get(I.getOperand(1));
        switch (I.getOpcode()) {
        default:
            assert(0);
        case Instruction::Add:
            return Solver->mkBVAdd(L, R);
        case Instruction::Sub:
            return Solver->mkBVSub(L, R);
        case Instruction::Mul:
            return Solver->mkBVMul(L, R);
        case Instruction::UDiv:
            return Solver->mkBVUDiv(L, R);
        case Instruction::SDiv:
            return Solver->mkBVSDiv(L, R);
        case Instruction::URem:
            return Solver->mkBVURem(L, R);
        case Instruction::SRem:
            return Solver->mkBVSRem(L, R);
        case Instruction::Shl:
            return Solver->mkBVShl(L, R);
        case Instruction::LShr:
            return Solver->mkBVLshr(L, R);
        case Instruction::AShr:
            return Solver->mkBVAshr(L, R);
        case Instruction::And:
            return Solver->mkBVAnd(L, R);
        case Instruction::Or:
            return Solver->mkBVOr(L, R);
        case Instruction::Xor:
            return Solver->mkBVXor(L, R);
        }
    }

    SMTExprRef visitICmpInst(ICmpInst &I) {
        SMTExprRef L = get(I.getOperand(0)), R = get(I.getOperand(1));
        switch (I.getPredicate()) {
        default:
            assert(0);
        case CmpInst::ICMP_EQ:
            return bool2bv(Solver, Solver->mkEqual(L, R));
        case CmpInst::ICMP_NE:
            return bool2bv(Solver, Solver->mkNot(Solver->mkEqual(L, R)));
        case CmpInst::ICMP_SGE:
            return bool2bv(Solver, Solver->mkBVSge(L, R));
        case CmpInst::ICMP_SGT:
            return bool2bv(Solver, Solver->mkBVSgt(L, R));
        case CmpInst::ICMP_SLE:
            return bool2bv(Solver, Solver->mkBVSle(L, R));
        case CmpInst::ICMP_SLT:
            return bool2bv(Solver, Solver->mkBVSlt(L, R));
        case CmpInst::ICMP_UGE:
            return bool2bv(Solver, Solver->mkBVUge(L, R));
        case CmpInst::ICMP_UGT:
            return bool2bv(Solver, Solver->mkBVUgt(L, R));
        case CmpInst::ICMP_ULE:
            return bool2bv(Solver, Solver->mkBVUle(L, R));
        case CmpInst::ICMP_ULT:
            return bool2bv(Solver, Solver->mkBVUlt(L, R));
        }
    }

    SMTExprRef visitSelectInst(SelectInst &I) {
        return Solver->mkIte(bv2bool(Solver, get(I.getCondition())),
                             get(I.getTrueValue()), get(I.getFalseValue()));
    }

    SMTExprRef visitExtractValueInst(ExtractValueInst &I) {
        IntrinsicInst *II = dyn_cast<IntrinsicInst>(I.getAggregateOperand());
        if (!II || II->getCalledFunction()->getName().find(".with.overflow.") ==
                       StringRef::npos)
            return mkBVFresh(&I);
        SMTExprRef L = get(II->getArgOperand(0));
        SMTExprRef R = get(II->getArgOperand(1));
        assert(I.getNumIndices() == 1);
        switch (I.getIndices()[0]) {
        default:
            II->dump();
            assert(0 && "Unknown overflow!");
        case 0:
            switch (II->getIntrinsicID()) {
            default:
                II->dump();
                assert(0 && "Unknown overflow!");
            case Intrinsic::sadd_with_overflow:
            case Intrinsic::uadd_with_overflow:
                return Solver->mkBVAdd(L, R);
            case Intrinsic::ssub_with_overflow:
            case Intrinsic::usub_with_overflow:
                return Solver->mkBVSub(L, R);
            case Intrinsic::smul_with_overflow:
            case Intrinsic::umul_with_overflow:
                return Solver->mkBVMul(L, R);
            }
        case 1:
            switch (II->getIntrinsicID()) {
            default:
                II->dump();
                assert(0 && "Unknown overflow!");
            case Intrinsic::sadd_with_overflow:
                return mkBVSAddOverflow(Solver, L, R);
            case Intrinsic::uadd_with_overflow:
                return mkBVUAddOverflow(Solver, L, R);
            case Intrinsic::ssub_with_overflow:
                return mkBVSSubOverflow(Solver, L, R);
            case Intrinsic::usub_with_overflow:
                return mkBVUSubOverflow(Solver, L, R);
            case Intrinsic::smul_with_overflow:
                return mkBVSMulOverflow(Solver, L, R);
            case Intrinsic::umul_with_overflow:
                return mkBVUMulOverflow(Solver, L, R);
            }
        }
        assert(I.getIndices()[0] == 1 && "FIXME!");
    }

    SMTExprRef visitGetElementPtrInst(GetElementPtrInst &I) {
        return visitGEPOperator(cast<GEPOperator>(I));
    }

    SMTExprRef visitGEPOperator(GEPOperator &GEP) {
        unsigned PtrSize =
            DL->getPointerSizeInBits(/*GEP.getPointerAddressSpace()*/);
        // Start from base.
        SMTExprRef Offset = get(GEP.getPointerOperand());
        APInt ConstOffset = APInt::getNullValue(PtrSize);

        auto GTI = gep_type_begin(GEP);
        for (auto i = GEP.idx_begin(), e = GEP.idx_end(); i != e; ++i, ++GTI) {
            Value *V = *i;
            // Skip zero index.
            ConstantInt *C = dyn_cast<ConstantInt>(V);
            if (C && C->isZero())
                continue;
            // For a struct, add the member offset.
            if (StructType *ST = GTI.getStructTypeOrNull()) {
                assert(C);
                unsigned FieldNo = C->getZExtValue();
                ConstOffset =
                    ConstOffset +
                    DL->getStructLayout(ST)->getElementOffset(FieldNo);
                continue;
            }
            // For an array, add the scaled element offset.
            APInt ElemSize(PtrSize, DL->getTypeAllocSize(GTI.getIndexedType()));
            if (C) {
                // GEP index can be sign-extended.
                ConstOffset += ElemSize * C->getValue().sextOrTrunc(PtrSize);
                continue;
            }
            SMTExprRef SIdx = get(V);
            unsigned IdxSize = Solver->getSort(SIdx)->getBitvectorSortSize();
            // Sometimes a 64-bit GEP's index is 32-bit.
            if (IdxSize != PtrSize) {
                SIdx = IdxSize < PtrSize
                           ? Solver->mkBVSignExt(PtrSize - IdxSize, SIdx)
                           : Solver->mkBVExtract(PtrSize - 1, 0, SIdx);
            }
            SMTExprRef SElemSize = mkBV(Solver, ElemSize);
            SMTExprRef LocalOffset = Solver->mkBVMul(SIdx, SElemSize);
            Offset = Solver->mkBVAdd(Offset, LocalOffset);
        }

        if (!ConstOffset)
            return Offset;

        // Merge constant offset.
        return Solver->mkBVAdd(Offset, mkBV(Solver, ConstOffset));
    }

    SMTExprRef visitBitCastInst(BitCastInst &I) {
        Value *V = I.getOperand(0);
        // V can be floating point.
        if (!VG->isAnalyzable(V))
            return mkBVFresh(&I);
        return get(V);
    }

    SMTExprRef visitPtrToIntInst(PtrToIntInst &I) {
        Value *V = I.getOperand(0);
        SMTExprRef E = get(V);
        unsigned PtrSize = getBitWidth(V);
        unsigned IntSize = getBitWidth(&I);
        if (IntSize > PtrSize)
            return Solver->mkBVZeroExt(IntSize - PtrSize, E);
        if (IntSize < PtrSize)
            return Solver->mkBVExtract(IntSize - 1, 0, E);
        return E;
    }

  private:
    SMTExprRef get(Value *V) { return VG->get(V); }

    unsigned getBitWidth(Type *T) const { return DL->getTypeSizeInBits(T); }

    unsigned getBitWidth(Value *V) const { return getBitWidth(V->getType()); }

    SMTExprRef mkBVFresh(Value *V) {
        std::string Name;
        {
            raw_string_ostream OS(Name);
            if (V->hasName())
                OS << V->getName();
            // Make name unique, e.g., undef.
            OS << "@" << V;
        }
        SMTSortRef Sort = Solver->getBitvectorSort(getBitWidth(V));
        return Solver->mkSymbol(Name.c_str(), Sort);
    }

    void addRangeConstraints(SMTExprRef E, MDNode *MD) {
        // !range comes in pairs.
        unsigned n = MD->getNumOperands();
        assert(n % 2 == 0);
        for (unsigned i = 0; i != n; i += 2) {
            auto LoOp = dyn_cast<ConstantAsMetadata>(MD->getOperand(i));
            if (!LoOp)
                report_fatal_error("Bit set element offset must be a constant");
            const APInt &Lo = cast<ConstantInt>(LoOp->getValue())->getValue();

            auto HiOp = dyn_cast<ConstantAsMetadata>(MD->getOperand(i + 1));
            if (!HiOp)
                report_fatal_error("Bit set element offset must be a constant");
            const APInt &Hi = cast<ConstantInt>(HiOp->getValue())->getValue();

            // const APInt &Lo =
            // cast<ConstantInt>(MD->getOperand(i))->getValue(); const APInt &Hi
            // = cast<ConstantInt>(MD->getOperand(i + 1))->getValue();

            // Ignore empty or full set.
            if (Lo == Hi)
                continue;
            SMTExprRef Cmp0 = nullptr, Cmp1 = nullptr, Cond;
            // Ignore >= 0.
            if (!!Lo)
                Cmp0 = Solver->mkBVUge(E, mkBV(Solver, Lo));
            // Note that (< Hi) is not always correct.
            // Need to ignore Hi == 0 (i.e., <= UMAX) or use (<= Hi - 1).
            if (!!Hi) {
                Cmp1 = Solver->mkBVUlt(E, mkBV(Solver, Hi));
            }
            if (!Cmp0) {
                Cond = Cmp1;
            } else if (!Cmp1) {
                Cond = Cmp0;
            } else {
                if (Lo.ule(Hi)) // [Lo, Hi).
                    Cond = Solver->mkBVAnd(Cmp0, Cmp1);
                else // Wrap: [Lo, UMAX] union [0, Hi).
                    Cond = Solver->mkBVOr(Cmp0, Cmp1);
            }
            addBVConstraint(Solver, Cond);
        }
    }
#undef Solver
#undef DL
};

namespace {
ValueVisitor VV;
}

SMTExprRef ValueGen::get(Value *V) {
    if (Cache.count(V))
        return Cache.lookup(V);
    SMTExprRef E = VV.analyze(this, V);
    Cache[V] = E;
    assert(E);
    return E;
}

bool ValueGen::isAnalyzable(Type *T) {
    return T->isIntegerTy() || T->isPointerTy() || T->isFunctionTy();
}

bool ValueGen::isAnalyzable(Value *V) { return isAnalyzable(V->getType()); }
