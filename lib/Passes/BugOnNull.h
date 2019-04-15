#pragma once

#include "BugOn.h"
#include <llvm/ADT/SmallPtrSet.h>

class BugOnNullPass : public BugOnPass {
    using SmallPtrSet = llvm::SmallPtrSet<llvm::Value *, 32>;

	SmallPtrSet Visited;

	bool runOnInstruction(llvm::Instruction *) override;

public:
    static llvm::StringRef name() { return "BugOnNullPass"; }
};
