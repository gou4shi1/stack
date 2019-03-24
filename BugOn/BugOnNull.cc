#define DEBUG_TYPE "bugon-null"

#include "BugOnNull.h"

using namespace llvm;

namespace stack {

bool BugOnNullPass::runOnInstruction(Instruction *I) {
    if (I->isTerminator()) {
		Visited.clear();
		return false;
    }
	Value *Base = getNonvolatileBaseAddress(I);
	if (!Base) return false;
	if (!Visited.insert(Base).second) return false;
	return insert(createIsNull(Base), "null pointer dereference");
}

} // end stack namespace
