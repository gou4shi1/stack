#pragma once

#include "BugOn.h"
#include <llvm/ADT/SmallPtrSet.h>

namespace stack {

class BugOnNullPass : public BugOnPass {
    typedef llvm::SmallPtrSet<Value *, 32> SmallPtrSet;

	bool runOnInstruction(Instruction *) override;

	SmallPtrSet Visited;
};

} // end stack namespace
