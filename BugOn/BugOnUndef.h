#pragma once

#include "BugOn.h"

namespace stack {

class BugOnUndefPass : public BugOnPass {
	bool runOnInstruction(Instruction *) override;
};

} // end stack namespace
