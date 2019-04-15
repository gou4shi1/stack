#pragma once

#include <llvm/IR/PassManager.h>

class ElimAssertPass {
    bool safeBB(llvm::BasicBlock *BB);
    void dropBB(llvm::Function &F, llvm::BasicBlock *BB);

public:
    llvm::PreservedAnalyses run(llvm::Function &, llvm::FunctionAnalysisManager &);
    static llvm::StringRef name() { return "ElimAssertPass"; }
};
