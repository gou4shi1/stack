#include "SMTHelper.h"
#include <llvm/Support/SMTAPI.h>
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/APSInt.h>

using namespace llvm;

SMTExprRef mkBV(SMTSolverRef SMT, const APInt &Int) {
    return SMT->mkBitvector(APSInt(Int), Int.getBitWidth());
}

SMTExprRef mkBVTrue(SMTSolverRef SMT) {
    return mkBV(SMT, APInt(1, 1));
}

SMTExprRef mkBVFalse(SMTSolverRef SMT) {
    return mkBV(SMT, APInt(1, 0));
}

SMTExprRef bool2bv(SMTSolverRef SMT, const SMTExprRef &Bool) {
    return SMT->mkIte(Bool, mkBVTrue(SMT), mkBVFalse(SMT));
}

SMTExprRef bv2bool(SMTSolverRef SMT, const SMTExprRef &BV) {
    return SMT->mkEqual(BV, mkBVTrue(SMT));
}

SMTExprRef mkBVSAddOverflow(SMTSolverRef SMT, const SMTExprRef &LHS, const SMTExprRef &RHS) {
    return bool2bv(SMT, SMT->mkOr(SMT->mkNot(SMT->mkBVAddNoOverflow(LHS, RHS, true)), SMT->mkNot(SMT->mkBVAddNoUnderflow(LHS, RHS))));
}

SMTExprRef mkBVUAddOverflow(SMTSolverRef SMT, const SMTExprRef &LHS, const SMTExprRef &RHS) {
    return bool2bv(SMT, SMT->mkNot(SMT->mkBVAddNoOverflow(LHS, RHS, false)));
}

SMTExprRef mkBVSSubOverflow(SMTSolverRef SMT, const SMTExprRef &LHS, const SMTExprRef &RHS) {
    return bool2bv(SMT, SMT->mkOr(SMT->mkNot(SMT->mkBVSubNoOverflow(LHS, RHS)), SMT->mkNot(SMT->mkBVSubNoUnderflow(LHS, RHS, true))));
}

SMTExprRef mkBVUSubOverflow(SMTSolverRef SMT, const SMTExprRef &LHS, const SMTExprRef &RHS) {
    return bool2bv(SMT, SMT->mkNot(SMT->mkBVSubNoUnderflow(LHS, RHS, false)));
}

SMTExprRef mkBVSMulOverflow(SMTSolverRef SMT, const SMTExprRef &LHS, const SMTExprRef &RHS) {
    return bool2bv(SMT, SMT->mkOr(SMT->mkNot(SMT->mkBVMulNoOverflow(LHS, RHS, true)), SMT->mkNot(SMT->mkBVMulNoUnderflow(LHS, RHS))));
}

SMTExprRef mkBVUMulOverflow(SMTSolverRef SMT, const SMTExprRef &LHS, const SMTExprRef &RHS) {
    return bool2bv(SMT, SMT->mkNot(SMT->mkBVMulNoOverflow(LHS, RHS, false)));
}
