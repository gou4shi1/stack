#pragma once

#include "BugOn.h"

namespace llvm {

class TargetLibraryInfo;
class DominatorTree;
class CallSite;

} // namespace llvm

class BugOnFreePass : public BugOnPass {
    llvm::TargetLibraryInfo *TLI;
    llvm::DominatorTree *DT;
    llvm::SmallPtrSet<llvm::Use *, 4> FreePtrs; // Use is a <call, arg> pair.

    llvm::Use *extractFree(llvm::CallSite CS);

    void initialize(llvm::Function &,
                     llvm::FunctionAnalysisManager &) override;
    bool runOnInstruction(llvm::Instruction *) override;

  public:
    static llvm::StringRef name() { return "BugOnFreePass"; }
};
