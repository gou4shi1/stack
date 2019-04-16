#pragma once

#include <llvm/ADT/Optional.h>

namespace llvm {

class APInt;
class APSInt;
class SMTExpr;
class SMTSolver;
using SMTExprRef = const SMTExpr *;
using SMTSolverRef = std::shared_ptr<SMTSolver>;

} // namespace llvm

llvm::SMTExprRef mkBV(llvm::SMTSolverRef &, const llvm::APInt &);
llvm::SMTExprRef mkBVTrue(llvm::SMTSolverRef &);
llvm::SMTExprRef mkBVFalse(llvm::SMTSolverRef &);
llvm::SMTExprRef bool2bv(llvm::SMTSolverRef &, const llvm::SMTExprRef &);
llvm::SMTExprRef bv2bool(llvm::SMTSolverRef &, const llvm::SMTExprRef &);
llvm::SMTExprRef mkBVSAddOverflow(llvm::SMTSolverRef &,
                                  const llvm::SMTExprRef &,
                                  const llvm::SMTExprRef &);
llvm::SMTExprRef mkBVSAddOverflow(llvm::SMTSolverRef &,
                                  const llvm::SMTExprRef &,
                                  const llvm::SMTExprRef &);
llvm::SMTExprRef mkBVUAddOverflow(llvm::SMTSolverRef &,
                                  const llvm::SMTExprRef &,
                                  const llvm::SMTExprRef &);
llvm::SMTExprRef mkBVSSubOverflow(llvm::SMTSolverRef &,
                                  const llvm::SMTExprRef &,
                                  const llvm::SMTExprRef &);
llvm::SMTExprRef mkBVUSubOverflow(llvm::SMTSolverRef &,
                                  const llvm::SMTExprRef &,
                                  const llvm::SMTExprRef &);
llvm::SMTExprRef mkBVSMulOverflow(llvm::SMTSolverRef &,
                                  const llvm::SMTExprRef &,
                                  const llvm::SMTExprRef &);
llvm::SMTExprRef mkBVUMulOverflow(llvm::SMTSolverRef &,
                                  const llvm::SMTExprRef &,
                                  const llvm::SMTExprRef &);

llvm::Optional<bool> queryBV(llvm::SMTSolverRef &, const llvm::SMTExprRef &);
void addBVConstraint(llvm::SMTSolverRef &, const llvm::SMTExprRef &);
