#pragma once

#include <llvm/ADT/DenseMap.h>
#include <memory>

namespace llvm {
    class DataLayout;
    class Value;
    class Type;
    class SMTExpr;
    class SMTSolver;
    using SMTExprRef = const SMTExpr *;
    using SMTSolverRef = std::shared_ptr<SMTSolver>;
}

class ValueGen {
    using ValueExprMap = llvm::DenseMap<llvm::Value *, llvm::SMTExprRef>;
    using iterator = ValueExprMap::iterator;

	ValueExprMap Cache;

public:
	const llvm::DataLayout &DL;
    llvm::SMTSolverRef Solver;

    ValueGen(const llvm::DataLayout &DL, llvm::SMTSolverRef Solver) : DL(DL), Solver(Solver) {}

    llvm::SMTExprRef get(llvm::Value *);

	iterator begin() { return Cache.begin(); }
	iterator end() { return Cache.end(); }

    static bool isAnalyzable(llvm::Type *T);
    static bool isAnalyzable(llvm::Value *V);
};
