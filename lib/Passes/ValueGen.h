#pragma once

#include <llvm/ADT/DenseMap.h>

namespace llvm {
    class DataLayout;
    class Value;
    class Type;
    class SMTExpr;
    using SMTExprRef = const SMTExpr *;
    class SMTSolver;
    using SMTSolverRef = std::shared_ptr<SMTSolver>;
}

class ValueGen {
    using ValueExprMap = llvm::DenseMap<llvm::Value *, llvm::SMTExprRef>;
    using iterator = ValueExprMap::iterator;

	ValueExprMap Cache;
    llvm::SMTSolverRef Solver;
	const llvm::DataLayout *DL;

    friend class ValueVisitor;

public:
    ValueGen(llvm::SMTSolverRef Solver, const llvm::DataLayout *DL) : Solver(Solver), DL(DL) {}

    llvm::SMTExprRef get(llvm::Value *);

	iterator begin() { return Cache.begin(); }
	iterator end() { return Cache.end(); }

    static bool isAnalyzable(llvm::Type *T);
    static bool isAnalyzable(llvm::Value *V);
};

using ValueGenRef = std::shared_ptr<ValueGen>;
