#pragma once

#include "BugOn.h"

namespace stack {

class BugOnIntPass : public BugOnPass {
	bool runOnInstruction(Instruction *) override;
	bool visitShiftOperator(IntegerType *, Value *R, const char *Bug);
};

} // end stack namespace
