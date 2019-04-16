#pragma once

#include "BugOn.h"
#include <llvm/Analysis/MemoryBuiltins.h>

namespace llvm {

class TargetLibraryInfo;

} // namespace llvm

class BugOnBoundsPass : public BugOnPass {
    llvm::TargetLibraryInfo *TLI;
    std::unique_ptr<llvm::ObjectSizeOffsetEvaluator> ObjSizeEval;

    void getAnalysis(llvm::Function &,
                     llvm::FunctionAnalysisManager &) override;
    bool runOnInstruction(llvm::Instruction *) override;

  public:
    static llvm::StringRef name() { return "BugOnBoundsPass"; }
};
