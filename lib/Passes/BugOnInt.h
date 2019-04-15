#pragma once

#include "BugOn.h"

class BugOnIntPass : public BugOnPass {
	bool runOnInstruction(llvm::Instruction *) override;
	bool visitShiftOperator(llvm::IntegerType *, llvm::Value *R, const char *Bug);

public:
    static llvm::StringRef name() { return "BugOnIntPass"; }
};
