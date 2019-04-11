#pragma once

#include <llvm/IR/PassManager.h>

namespace llvm {
    class MemoryDependenceResults;
    class TargetLibraryInfo;
    class AAResults;
    using AliasAnalysis = AAResults;
    class LoadInst;
}

class LoadElimPass {
	llvm::AliasAnalysis *AA;
	llvm::MemoryDependenceResults *MemDep;
	llvm::TargetLibraryInfo *TLI;

	bool merge(llvm::LoadInst *);

public:
    llvm::PreservedAnalyses run(llvm::Function &, llvm::FunctionAnalysisManager &);
    static llvm::StringRef name() { return "load-elim"; }
};
