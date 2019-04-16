#pragma once

#include "BugFree.h"

namespace llvm {
    class TargetLibraryInfo;
    class ScalarEvolution;
    class ICmpInst;
}

class BugFreeAlgebraSimplifyPass : public BugFreePass {
    llvm::TargetLibraryInfo *TLI;
    llvm::ScalarEvolution *SE;

    llvm::PreservedAnalyses runOnFunction(llvm::Function &, llvm::FunctionAnalysisManager &) override;

	bool visitICmpInst(llvm::ICmpInst *I);
	bool checkEqv(llvm::ICmpInst *Old, llvm::ICmpInst *New);

public:
    static llvm::StringRef name() { return "BugFreeAlgebraSimplifyPass"; }
};
