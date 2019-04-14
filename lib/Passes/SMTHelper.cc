#include "SMTHelper.h"
#include <llvm/ADT/APInt.h>
#include <llvm/ADT/APSInt.h>
#include <llvm/Support/SMTAPI.h>

using namespace llvm;

SMTExprRef mkBV(SMTSolverRef &Solver, const APInt &Int) {
    return Solver->mkBitvector(APSInt(Int), Int.getBitWidth());
}

SMTExprRef mkBVTrue(SMTSolverRef &Solver) { return mkBV(Solver, APInt(1, 1)); }

SMTExprRef mkBVFalse(SMTSolverRef &Solver) { return mkBV(Solver, APInt(1, 0)); }

SMTExprRef bool2bv(SMTSolverRef &Solver, const SMTExprRef &Bool) {
    return Solver->mkIte(Bool, mkBVTrue(Solver), mkBVFalse(Solver));
}

SMTExprRef bv2bool(SMTSolverRef &Solver, const SMTExprRef &BV) {
    return Solver->mkEqual(BV, mkBVTrue(Solver));
}

SMTExprRef mkBVSAddOverflow(SMTSolverRef &Solver, const SMTExprRef &LHS,
                            const SMTExprRef &RHS) {
    return bool2bv(
        Solver,
        Solver->mkOr(Solver->mkNot(Solver->mkBVAddNoOverflow(LHS, RHS, true)),
                     Solver->mkNot(Solver->mkBVAddNoUnderflow(LHS, RHS))));
}

SMTExprRef mkBVUAddOverflow(SMTSolverRef &Solver, const SMTExprRef &LHS,
                            const SMTExprRef &RHS) {
    return bool2bv(Solver,
                   Solver->mkNot(Solver->mkBVAddNoOverflow(LHS, RHS, false)));
}

SMTExprRef mkBVSSubOverflow(SMTSolverRef &Solver, const SMTExprRef &LHS,
                            const SMTExprRef &RHS) {
    return bool2bv(
        Solver, Solver->mkOr(
                    Solver->mkNot(Solver->mkBVSubNoOverflow(LHS, RHS)),
                    Solver->mkNot(Solver->mkBVSubNoUnderflow(LHS, RHS, true))));
}

SMTExprRef mkBVUSubOverflow(SMTSolverRef &Solver, const SMTExprRef &LHS,
                            const SMTExprRef &RHS) {
    return bool2bv(Solver,
                   Solver->mkNot(Solver->mkBVSubNoUnderflow(LHS, RHS, false)));
}

SMTExprRef mkBVSMulOverflow(SMTSolverRef &Solver, const SMTExprRef &LHS,
                            const SMTExprRef &RHS) {
    return bool2bv(
        Solver,
        Solver->mkOr(Solver->mkNot(Solver->mkBVMulNoOverflow(LHS, RHS, true)),
                     Solver->mkNot(Solver->mkBVMulNoUnderflow(LHS, RHS))));
}

SMTExprRef mkBVUMulOverflow(SMTSolverRef &Solver, const SMTExprRef &LHS,
                            const SMTExprRef &RHS) {
    return bool2bv(Solver,
                   Solver->mkNot(Solver->mkBVMulNoOverflow(LHS, RHS, false)));
}

Optional<bool> queryBV(llvm::SMTSolverRef &Solver, const llvm::SMTExprRef &BV) {
    Solver->push();
    Solver->addConstraint(bv2bool(Solver, BV));
    Optional<bool> res = Solver->check();
    Solver->pop();
    return res;
}

void addBVConstraint(llvm::SMTSolverRef &Solver, const llvm::SMTExprRef &Q) {
    Solver->addConstraint(bv2bool(Solver, Q));
}
