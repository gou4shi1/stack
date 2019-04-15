#pragma once

#include <llvm/IR/PassManager.h>

class NullCheckElimPass {
	bool MarkNullCheckEmitByLLVM(llvm::BasicBlock &);

public:
    llvm::PreservedAnalyses run(llvm::Function &, llvm::FunctionAnalysisManager &);
    static llvm::StringRef name() { return "NullCheckElimPass"; }
};
