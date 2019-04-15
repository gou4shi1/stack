#pragma once

#include <llvm/IR/PassManager.h>

namespace llvm {
    class Function;
    class DominatorTree;
}

class CFGElimPass {
	llvm::DominatorTree *DT;

	bool removeUnreachableBB(llvm::Function &);

public:
    llvm::PreservedAnalyses run(llvm::Function &, llvm::FunctionAnalysisManager &);
    static llvm::StringRef name() { return "CFGElimPass"; }
};
