#pragma once

#include "BugOn.h"

class BugOnUndefPass : public BugOnPass {
    bool runOnInstruction(llvm::Instruction *) override;

  public:
    static llvm::StringRef name() { return "BugOnUndefPass"; }
};
