#pragma once

namespace llvm {

class Value;
class Instruction;

} // namespace llvm

bool clearDebugLoc(llvm::Value *);
bool recursivelyClearDebugLoc(llvm::Value *);

// Return if I has a non-inlined debug location.
bool hasSingleDebugLocation(const llvm::Instruction *I);
