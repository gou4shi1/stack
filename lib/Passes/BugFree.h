#pragma once

#include "Diagnostic.h"
#include "PathGen.h"
#include "ValueGen.h"
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/Optional.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/PassManager.h>

// TODO: Benchmark
// #define BENCHMARK(e) if (BenchmarkFlag) { e; }

// extern bool BenchmarkFlag;

llvm::Function *getBugOn(const llvm::Module *);
llvm::Function *getOrInsertBugOn(llvm::Module *);
class BugOnInst;

class BugFreePass {
    llvm::Function *BugOn;
    llvm::DenseMap<llvm::BasicBlock *, bool> isBlockInLoop;
    llvm::SmallVector<BugOnInst *, 8> Assertions;
    llvm::PostDominatorTree *PDT;
    void *Buffer;

    bool initialize(llvm::Function &, llvm::FunctionAnalysisManager &);
    void reset();
    void calculateBackedgesAndInLoopBlocks(llvm::Function &);

  public:
    BugFreePass();
    BugFreePass(const BugFreePass &);
    ~BugFreePass();
    llvm::PreservedAnalyses run(llvm::Function &,
                                llvm::FunctionAnalysisManager &);

  protected:
    const llvm::DataLayout *DL;
    llvm::DominatorTree *DT;
    llvm::SmallVector<PathGen::Edge, 32> Backedges;
    llvm::SMTSolverRef Solver;
    ValueGenRef VG;
    PathGenRef PG;
    Diagnostic Diag;

    virtual llvm::PreservedAnalyses
    runOnFunction(llvm::Function &, llvm::FunctionAnalysisManager &) = 0;

    // Call if CFG has changed.
    void recalculate(llvm::Function &F);
    // Return bug-free assertion.
    llvm::SMTExprRef computeBugFreeDelta(llvm::SmallVectorImpl<BugOnInst *> &);
    llvm::SMTExprRef getBugFreeDelta(llvm::BasicBlock *);
    llvm::Optional<bool> queryWithBugFreeDelta(llvm::SMTExprRef E,
                                               llvm::SMTExprRef Delta);
    void printMinimalAssertions();
};
