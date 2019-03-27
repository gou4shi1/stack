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
}

class ValueGen;

class PathGen {
	using BBExprMap = llvm::DenseMap<llvm::BasicBlock *, llvm::SMTExprRef>;
	using iterator = BBExprMap::iterator;
	using Edge = std::pair<const llvm::BasicBlock *, const llvm::BasicBlock *>;
	using EdgeVec = llvm::SmallVectorImpl<Edge>;

	ValueGen &VG;
	const EdgeVec &Backedges;
	llvm::DominatorTree *DT;
	BBExprMap Cache;

public:
	PathGen(ValueGen &VG, const EdgeVec &BE) : VG(VG), Backedges(BE), DT(NULL) {}
	PathGen(ValueGen &VG, const EdgeVec &BE, llvm::DominatorTree &DT) : VG(VG), Backedges(BE), DT(&DT) {}

    llvm::SMTExprRef get(llvm::BasicBlock *);

private:
	bool isBackedge(llvm::BasicBlock *, llvm::BasicBlock *);
	llvm::SMTExprRef getTermGuard(llvm::Instruction *I, llvm::BasicBlock *BB);
    llvm::SMTExprRef getTermGuard(llvm::BranchInst *I, llvm::BasicBlock *BB);
    llvm::SMTExprRef getTermGuard(llvm::SwitchInst *I, llvm::BasicBlock *BB);
    llvm::SMTExprRef getPHIGuard(llvm::BasicBlock *BB, llvm::BasicBlock *Pred);
};
