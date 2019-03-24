#pragma once

#include "BugOn.h"

namespace stack {

class BugOnUndefPass : public BugOnPass {
	bool runOnInstruction(Instruction *) override;

public:
    static StringRef name() { return "bugon-undef"; }
};

} // end stack namespace
