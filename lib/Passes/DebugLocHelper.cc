#include <llvm/IR/Instruction.h>

using namespace llvm;

bool clearDebugLoc(Value *V) {
    Instruction *I = dyn_cast<Instruction>(V);
    if (!I || !I->getDebugLoc())
        return false;
    I->setDebugLoc(DebugLoc());
    return true;
}

bool recursivelyClearDebugLoc(Value *V) {
    Instruction *I = dyn_cast<Instruction>(V);
    if (!I || !I->getDebugLoc())
        return false;
    I->setDebugLoc(DebugLoc());
    for (auto &U : I->operands()) {
        if (U->hasOneUse())
            recursivelyClearDebugLoc(U);
    }
    return true;
}

bool hasSingleDebugLocation(Instruction *I) {
    const DebugLoc &DbgLoc = I->getDebugLoc();
    // Skip inserted instructions without debugging information.
    if (!DbgLoc)
        return false;
    // Skip inlined instructions.
    if (DbgLoc.getInlinedAt())
        return false;
    // Macro-expanded code.
    if (I->getMetadata("macro"))
        return false;
    return true;
}
