#pragma once

#include "BugFree.h"

namespace llvm {

class DomTreeUpdater;

}

class BugFreeDCEPass : public BugFreePass {

    llvm::PreservedAnalyses
    runOnFunction(llvm::Function &, llvm::FunctionAnalysisManager &) override;

    bool shouldCheck(llvm::BasicBlock *, llvm::DomTreeUpdater *, bool &);
    bool check(llvm::BasicBlock *, llvm::DomTreeUpdater *);
    void ReportAndReplace(llvm::BasicBlock *);
    void markAsDead(llvm::BasicBlock *BB, llvm::DomTreeUpdater *);

  public:
    static llvm::StringRef name() { return "BugFreeDCEPass"; }
};
