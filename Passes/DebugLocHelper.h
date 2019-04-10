#pragma once

namespace llvm {
    class Value;
    class Instruction;
}

bool clearDebugLoc(llvm::Value *);
bool recursivelyClearDebugLoc(llvm::Value *);

// Return if I has a non-inlined debug location.
bool hasSingleDebugLocation(llvm::Instruction *I);
