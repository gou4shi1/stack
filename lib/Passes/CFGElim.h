#pragma once

#include <llvm/IR/PassManager.h>

class CFGElimPass {
  public:
    llvm::PreservedAnalyses run(llvm::Function &,
                                llvm::FunctionAnalysisManager &);
    static llvm::StringRef name() { return "CFGElimPass"; }
};
