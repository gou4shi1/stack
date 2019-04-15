#pragma once

#include "BugFree.h"

namespace llvm {
    class TargetLibraryInfo;
    class ScalarEvolution;
    class ICmpInst;
}

class BugFreeAlgebraSimplyPass : public BugFreePass {
    llvm::TargetLibraryInfo *TLI;
    llvm::ScalarEvolution *SE;

	bool runOnFunction(llvm::Function &, llvm::FunctionAnalysisManager &) override;

	bool visitICmpInst(llvm::ICmpInst *I);
	bool checkEqv(llvm::ICmpInst *Old, llvm::ICmpInst *New);

public:
    static llvm::StringRef name() { return "BugFreeAlgebraSimplyPass"; }
};
