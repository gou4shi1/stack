#pragma once

#include "BugOn.h"

namespace stack {

class BugOnIntPass : public BugOnPass {
	bool runOnInstruction(Instruction *) override;
	bool visitShiftOperator(IntegerType *, Value *R, const char *Bug);

public:
    static StringRef name() { return "bugon-int"; }
};

} // end stack namespace
