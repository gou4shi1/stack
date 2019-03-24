#pragma once

#include "BugOn.h"

namespace stack {

class BugOnIntPass : public BugOnPass {
public:
    typedef llvm::IntegerType IntegerType;

    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) override;

private:
	bool runOnInstruction(Instruction *) override;
	bool visitShiftOperator(IntegerType *, Value *R, const char *Bug);
};

} // end stack namespace
