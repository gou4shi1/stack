#include "ValueGen.h"
#include <llvm/ADT/APInt.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/InstVisitor.h>
#include <llvm/IR/IntrinsicInst.h>
#include <llvm/IR/Operator.h>
#include <llvm/IR/GetElementPtrTypeIterator.h>
#include <llvm/Support/raw_ostream.h>
#include <assert.h>

using namespace llvm;

namespace {

#define SMT    VG->Solver
#define DL     VG->DL

class ValueVisitor : public InstVisitor<ValueVisitor, SMTExprRef> {
	ValueGen *VG;
    SMTExprRef BVTrue, BVFalse;

	SMTExprRef get(Value *V) {
		return VG->get(V);
	}

	unsigned getBitWidth(Type *T) const {
		return DL.getTypeSizeInBits(T);
	}

	unsigned getBitWidth(Value *V) const {
		return getBitWidth(V->getType());
	}

    SMTExprRef mkBV(const APInt &Int) {
        return SMT->mkBitvector(APSInt(Int), Int.getBitWidth());
    }

	SMTExprRef mkBVFresh(Value *V) {
		std::string Name;
		{
			raw_string_ostream OS(Name);
			if (V->hasName())
				OS << V->getName();
			// Make name unique, e.g., undef.
			OS << "@" << V;
		}
        SMTSortRef Sort = SMT->getBitvectorSort(getBitWidth(V));
        return SMT->mkSymbol(Name.c_str(), Sort);
	}

    SMTExprRef bool2bv(const SMTExprRef &Bool) {
        SMT->mkIte(Bool, BVTrue, BVFalse);
    }

    SMTExprRef bv2bool(const SMTExprRef &BV) {
        SMT->mkEqual(BV, BVTrue);
    }

    SMTExprRef mkBVSAddOverflow(const SMTExprRef &LHS, const SMTExprRef &RHS) {
        return bool2bv(SMT->mkOr(SMT->mkNot(SMT->mkBVAddNoOverflow(LHS, RHS, true)), SMT->mkNot(SMT->mkBVAddNoUnderflow(LHS, RHS))));
    }

    SMTExprRef mkBVUAddOverflow(const SMTExprRef &LHS, const SMTExprRef &RHS) {
        return bool2bv(SMT->mkNot(SMT->mkBVAddNoOverflow(LHS, RHS, false)));
    }

    SMTExprRef mkBVSSubOverflow(const SMTExprRef &LHS, const SMTExprRef &RHS) {
        return bool2bv(SMT->mkOr(SMT->mkNot(SMT->mkBVSubNoOverflow(LHS, RHS)), SMT->mkNot(SMT->mkBVSubNoUnderflow(LHS, RHS, true))));
    }

    SMTExprRef mkBVUSubOverflow(const SMTExprRef &LHS, const SMTExprRef &RHS) {
        return bool2bv(SMT->mkNot(SMT->mkBVSubNoUnderflow(LHS, RHS, false)));
    }

    SMTExprRef mkBVSMulOverflow(const SMTExprRef &LHS, const SMTExprRef &RHS) {
        return bool2bv(SMT->mkOr(SMT->mkNot(SMT->mkBVMulNoOverflow(LHS, RHS, true)), SMT->mkNot(SMT->mkBVMulNoUnderflow(LHS, RHS))));
    }

    SMTExprRef mkBVUMulOverflow(const SMTExprRef &LHS, const SMTExprRef &RHS) {
        return bool2bv(SMT->mkNot(SMT->mkBVMulNoOverflow(LHS, RHS, false)));
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

            auto HiOp = dyn_cast<ConstantAsMetadata>(MD->getOperand(i+1));
            if (!HiOp)
                report_fatal_error("Bit set element offset must be a constant");
            const APInt &Hi = cast<ConstantInt>(HiOp->getValue())->getValue();

            //const APInt &Lo = cast<ConstantInt>(MD->getOperand(i))->getValue();
            //const APInt &Hi = cast<ConstantInt>(MD->getOperand(i + 1))->getValue();
            
            // Ignore empty or full set.
            if (Lo == Hi)
                continue;
            SMTExprRef Cmp0 = nullptr, Cmp1 = nullptr, Cond;
            // Ignore >= 0.
            if (!!Lo)
                Cmp0 = SMT->mkBVUge(E, mkBV(Lo));
            // Note that (< Hi) is not always correct.
            // Need to ignore Hi == 0 (i.e., <= UMAX) or use (<= Hi - 1).
            if (!!Hi) {
                Cmp1 = SMT->mkBVUlt(E, mkBV(Hi));
            }
            if (!Cmp0) {
                Cond = Cmp1;
            } else if (!Cmp1) {
                Cond = Cmp0;
            } else {
                if (Lo.ule(Hi))	// [Lo, Hi).
                    Cond = SMT->mkBVAnd(Cmp0, Cmp1);
                else	        // Wrap: [Lo, UMAX] union [0, Hi).
                    Cond = SMT->mkBVOr(Cmp0, Cmp1);
            }
            SMT->addConstraint(Cond);
        }
    }

    public:
    ValueVisitor() = default;

	SMTExprRef analyze(ValueGen *TheVG, Value *V) {
        // initialize
        VG = TheVG;
        BVTrue = mkBV(APInt(1, 1));
        BVFalse = mkBV(APInt(1, 0));

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
            return mkBV(Int);
        }
        if (isa<ConstantPointerNull>(C)) {
            unsigned width = getBitWidth(C);
            return mkBV(APInt::getNullValue(width));
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
        return SMT->mkBVExtract(DstWidth - 1, 0, get(I.getOperand(0)));
	}

	SMTExprRef visitZExtInst(ZExtInst &I) {
		unsigned DstWidth = getBitWidth(I.getDestTy());
		unsigned SrcWidth = getBitWidth(I.getSrcTy());
        return SMT->mkBVZeroExt(DstWidth - SrcWidth, get(I.getOperand(0)));
	}

	SMTExprRef visitSExtInst(SExtInst &I) {
		unsigned DstWidth = getBitWidth(I.getDestTy());
		unsigned SrcWidth = getBitWidth(I.getSrcTy());
        return SMT->mkBVSignExt(DstWidth - SrcWidth, get(I.getOperand(0)));
	}

	SMTExprRef visitBinaryOperator(BinaryOperator &I) {
		SMTExprRef L = get(I.getOperand(0)), R = get(I.getOperand(1));
		switch (I.getOpcode()) {
        default: assert(0);
        case Instruction::Add:  return SMT->mkBVAdd(L, R);
        case Instruction::Sub:  return SMT->mkBVSub(L, R);
        case Instruction::Mul:  return SMT->mkBVMul(L, R);
        case Instruction::UDiv: return SMT->mkBVUDiv(L, R);
        case Instruction::SDiv: return SMT->mkBVSDiv(L, R);
        case Instruction::URem: return SMT->mkBVURem(L, R);
        case Instruction::SRem: return SMT->mkBVSRem(L, R);
        case Instruction::Shl:  return SMT->mkBVShl(L, R);
        case Instruction::LShr: return SMT->mkBVLshr(L, R);
        case Instruction::AShr: return SMT->mkBVAshr(L, R);
        case Instruction::And:  return SMT->mkBVAnd(L, R);
        case Instruction::Or:   return SMT->mkBVOr(L, R);
        case Instruction::Xor:  return SMT->mkBVXor(L, R);
		}
	}

	SMTExprRef visitICmpInst(ICmpInst &I) {
		SMTExprRef L = get(I.getOperand(0)), R = get(I.getOperand(1));
		switch (I.getPredicate()) {
        default: assert(0);
        case CmpInst::ICMP_EQ:  return bool2bv(SMT->mkEqual(L, R));
        case CmpInst::ICMP_NE:  return bool2bv(SMT->mkNot(SMT->mkEqual(L, R)));
        case CmpInst::ICMP_SGE: return bool2bv(SMT->mkBVSge(L, R));
        case CmpInst::ICMP_SGT: return bool2bv(SMT->mkBVSgt(L, R));
        case CmpInst::ICMP_SLE: return bool2bv(SMT->mkBVSle(L, R));
        case CmpInst::ICMP_SLT: return bool2bv(SMT->mkBVSlt(L, R));
        case CmpInst::ICMP_UGE: return bool2bv(SMT->mkBVUge(L, R));
        case CmpInst::ICMP_UGT: return bool2bv(SMT->mkBVUgt(L, R));
        case CmpInst::ICMP_ULE: return bool2bv(SMT->mkBVUle(L, R));
        case CmpInst::ICMP_ULT: return bool2bv(SMT->mkBVUlt(L, R));
		}
	}

	SMTExprRef visitSelectInst(SelectInst &I) {
		return SMT->mkIte(
			bv2bool(get(I.getCondition())),
			get(I.getTrueValue()),
			get(I.getFalseValue())
		);
	}

	SMTExprRef visitExtractValueInst(ExtractValueInst &I) {
		IntrinsicInst *II = dyn_cast<IntrinsicInst>(I.getAggregateOperand());
		if (!II || II->getCalledFunction()->getName().find(".with.overflow.")
				== StringRef::npos)
			return mkBVFresh(&I);
		SMTExprRef L = get(II->getArgOperand(0));
		SMTExprRef R = get(II->getArgOperand(1));
		assert(I.getNumIndices() == 1);
		switch (I.getIndices()[0]) {
		default: II->dump(); assert(0 && "Unknown overflow!");
		case 0:
			switch (II->getIntrinsicID()) {
			default: II->dump(); assert(0 && "Unknown overflow!");
			case Intrinsic::sadd_with_overflow:
			case Intrinsic::uadd_with_overflow:
				return SMT->mkBVAdd(L, R);
			case Intrinsic::ssub_with_overflow:
			case Intrinsic::usub_with_overflow:
				return SMT->mkBVSub(L, R);
			case Intrinsic::smul_with_overflow:
			case Intrinsic::umul_with_overflow:
				return SMT->mkBVMul(L, R);
			}
		case 1:
			switch (II->getIntrinsicID()) {
			default: II->dump(); assert(0 && "Unknown overflow!");
			case Intrinsic::sadd_with_overflow:
				return mkBVSAddOverflow(L, R);
			case Intrinsic::uadd_with_overflow:
				return mkBVUAddOverflow(L, R);
			case Intrinsic::ssub_with_overflow:
				return mkBVSSubOverflow(L, R);
			case Intrinsic::usub_with_overflow:
				return mkBVUSubOverflow(L, R);
			case Intrinsic::smul_with_overflow:
				return mkBVSMulOverflow(L, R);
			case Intrinsic::umul_with_overflow:
				return mkBVUMulOverflow(L, R);
			}
		}
		assert(I.getIndices()[0] == 1 && "FIXME!");

	}

	SMTExprRef visitGetElementPtrInst(GetElementPtrInst &I) {
		return visitGEPOperator(cast<GEPOperator>(I));
	}

	SMTExprRef visitGEPOperator(GEPOperator &GEP) {
		unsigned PtrSize = DL.getPointerSizeInBits(/*GEP.getPointerAddressSpace()*/);
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
			if (StructType *ST = dyn_cast<StructType>(GTI.getIndexedType())) {
				assert(C);
				unsigned FieldNo = C->getZExtValue();
				ConstOffset = ConstOffset + DL.getStructLayout(ST)->getElementOffset(FieldNo);
				continue;
			}
			// For an array, add the scaled element offset.
			APInt ElemSize(PtrSize, DL.getTypeAllocSize(GTI.getIndexedType()));
			if (C) {
				// GEP index can be sign-extended.
				ConstOffset += ElemSize * C->getValue().sextOrTrunc(PtrSize);
				continue;
			}
			SMTExprRef SIdx = get(V);
			unsigned IdxSize = SMT->getSort(SIdx)->getBitvectorSortSize();
			// Sometimes a 64-bit GEP's index is 32-bit.
			if (IdxSize != PtrSize) {
                SIdx = IdxSize < PtrSize ?
                    SMT->mkBVSignExt(PtrSize - IdxSize, SIdx) :
                    SMT->mkBVExtract(PtrSize - 1, 0, SIdx);
			}
            SMTExprRef SElemSize = mkBV(ElemSize);
			SMTExprRef LocalOffset = SMT->mkBVMul(SIdx, SElemSize);
			SMTExprRef Offset = SMT->mkBVAdd(Offset, LocalOffset);
		}

		if (!ConstOffset)
			return Offset;

		// Merge constant offset.
		return SMT->mkBVAdd(Offset, mkBV(ConstOffset));
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
			return SMT->mkBVZeroExt(IntSize - PtrSize, E);
		if (IntSize < PtrSize)
			return SMT->mkBVExtract(IntSize - 1, 0, E);
		return E;
	}

}VV;

#undef SMT
#undef DL

} // anonymous namespace

SMTExprRef ValueGen::get(Value *V) {
	SMTExprRef E = Cache.lookup(V);
	if (!E) {
		E = VV.analyze(this, V);
		Cache[V] = E;
	}
	assert(E);
	return E;
}

bool ValueGen::isAnalyzable(Type *T) {
    return T->isIntegerTy()
        || T->isPointerTy()
        || T->isFunctionTy();
}

bool ValueGen::isAnalyzable(Value *V) {
    return isAnalyzable(V->getType());
}
