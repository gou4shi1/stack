#pragma once

#include "BugFree.h"

class BugFreeConstantFoldPass : public BugFreePass {
    llvm::PreservedAnalyses
    runOnFunction(llvm::Function &, llvm::FunctionAnalysisManager &) override;

    llvm::Optional<int> foldConst(llvm::Instruction *);
    void ReportAndReplace(llvm::Instruction *, int);

  public:
    static llvm::StringRef name() { return "BugFreeConstantFoldPass"; }
};
