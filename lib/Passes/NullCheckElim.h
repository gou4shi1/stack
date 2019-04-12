#pragma once

#include <llvm/IR/PassManager.h>

namespace llvm {
    class Function;
    class BasicBlock;
    class MemoryDependenceResults;
    class TargetLibraryInfo;
    class MemorySSA;
    class LoopInfo;
    class DominatorTree;
}

class NullCheckElimPass {
	llvm::MemorySSA *MSSA;
	llvm::MemoryDependenceResults *MemDep;
	llvm::TargetLibraryInfo *TLI;
    llvm::LoopInfo *LI;
	llvm::DominatorTree *DT;

	bool removeUnreachableBB(llvm::Function &);
	bool DeleteNullCheckBB(llvm::BasicBlock *);

public:
    llvm::PreservedAnalyses run(llvm::Function &, llvm::FunctionAnalysisManager &);
    static llvm::StringRef name() { return "null-check-elim"; }
};
