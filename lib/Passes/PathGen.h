#pragma once

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallVector.h>

namespace llvm {

class DominatorTree;
class BasicBlock;
class BranchInst;
class SwitchInst;
class Instruction;
class SMTExpr;
using SMTExprRef = const SMTExpr *;
class SMTSolver;
using SMTSolverRef = std::shared_ptr<SMTSolver>;

} // namespace llvm

class ValueGen;
using ValueGenRef = std::shared_ptr<ValueGen>;

class PathGen {
  public:
    using BBExprMap = llvm::DenseMap<llvm::BasicBlock *, llvm::SMTExprRef>;
    using iterator = BBExprMap::iterator;
    using Edge = std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *>;
    using EdgeVec = llvm::SmallVectorImpl<Edge>;

  private:
    BBExprMap Cache;
    llvm::SMTSolverRef Solver;
    ValueGenRef VG;
    const llvm::DominatorTree *DT;
    const EdgeVec &Backedges;

  public:
    PathGen(llvm::SMTSolverRef Solver, ValueGenRef VG,
            const llvm::DominatorTree *DT, const EdgeVec &BE)
        : Solver(Solver), VG(VG), DT(DT), Backedges(BE) {}

    llvm::SMTExprRef get(llvm::BasicBlock *);

  private:
    bool isBackedge(llvm::BasicBlock *, llvm::BasicBlock *);
    llvm::SMTExprRef getTermGuard(llvm::Instruction *I, llvm::BasicBlock *BB);
    llvm::SMTExprRef getTermGuard(llvm::BranchInst *I, llvm::BasicBlock *BB);
    llvm::SMTExprRef getTermGuard(llvm::SwitchInst *I, llvm::BasicBlock *BB);
    llvm::SMTExprRef getPHIGuard(llvm::BasicBlock *BB, llvm::BasicBlock *Pred);
};

using PathGenRef = std::shared_ptr<PathGen>;
