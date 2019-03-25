#pragma once

#include <llvm/Support/SMTAPI.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/IR/DataLayout.h>

class ValueGen {
public:
	const llvm::DataLayout &DL;
    llvm::SMTSolverRef Solver;

    using ValueExprMap = llvm::DenseMap<llvm::Value *, llvm::SMTExprRef>;
    using iterator = ValueExprMap::iterator;

	ValueExprMap Cache;

    ValueGen(const llvm::DataLayout &DL, llvm::SMTSolverRef Slover) : DL(DL), Solver(Solver) {}

    llvm::SMTExprRef get(llvm::Value *);

	iterator begin() { return Cache.begin(); }
	iterator end() { return Cache.end(); }

    static bool isAnalyzable(llvm::Type *T);
    static bool isAnalyzable(llvm::Value *V);
};
