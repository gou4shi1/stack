#include "BugFree.h"
#include "BugOn.h"
#include "SMTHelper.h"
#include <llvm/ADT/Optional.h>
#include <llvm/ADT/SCCIterator.h>
#include <llvm/Analysis/CFG.h>
#include <llvm/Support/SMTAPI.h>
#include <sys/mman.h>

using namespace llvm;

static cl::opt<bool>
    IgnorePostOpt("ignore-bugon-post",
                  cl::desc("Ignore bugon conditions on post-dominators"),
                  cl::init(true));

static cl::opt<bool> MinBugOnOpt("min-bugon",
                                 cl::desc("Compute minimal bugon set"),
                                 cl::init(true));

static const size_t BUFFER_SIZE = 8192;
/*
bool BenchmarkFlag;

namespace {
        struct BenchmarkInit {
                BenchmarkInit() { BenchmarkFlag = !!::getenv("BENCHMARK"); }
        };
}

static BenchmarkInit X;
*/
BugFreePass::BugFreePass() {
    if (MinBugOnOpt)
        Buffer = mmap(NULL, BUFFER_SIZE, PROT_READ | PROT_WRITE,
                      MAP_ANON | MAP_SHARED, -1, 0);
}

BugFreePass::BugFreePass(const BugFreePass &P) {
    if (MinBugOnOpt)
        Buffer = mmap(NULL, BUFFER_SIZE, PROT_READ | PROT_WRITE,
                      MAP_ANON | MAP_SHARED, -1, 0);
}

BugFreePass::~BugFreePass() {
    if (Buffer)
        munmap(Buffer, BUFFER_SIZE);
}
/*
static std::string demangle(Function &F) {
        std::string Name = F.getName();
        char *s = abi::__cxa_demangle(Name.c_str(), NULL, NULL, NULL);
        if (s) {
                Name = s;
                free(s);
        }
        return Name;
}
*/
void BugFreePass::calculateBackedgesAndInLoopBlocks(Function &F) {
    Backedges.clear();
    isBlockInLoop.clear();
    FindFunctionBackedges(F, Backedges);
    if (Backedges.empty())
        return;
    // No need to calculate InLoopBlocks if post-dominators are ignored.
    if (IgnorePostOpt)
        return;
    for (auto i = scc_begin(&F), e = scc_end(&F); i != e; ++i) {
        if (i.hasLoop()) {
            for (const auto &j : *i)
                isBlockInLoop[j] = true;
        }
    }
}

bool BugFreePass::initialize(Function &F, FunctionAnalysisManager &FAM) {
    BugOn = getBugOn(F.getParent());
    if (!BugOn)
        return false;
    // DEBUG(dbgs() << "Analyzing " << demangle(F) << "\n");
    assert(BugOn->arg_size() == 1);
    assert(BugOn->arg_begin()->getType()->isIntegerTy(1));
    DL = &F.getParent()->getDataLayout();
    DT = &FAM.getResult<DominatorTreeAnalysis>(F);
    PDT = &FAM.getResult<PostDominatorTreeAnalysis>(F);
    calculateBackedgesAndInLoopBlocks(F);
    Solver = CreateZ3Solver();
    VG = std::make_shared<ValueGen>(Solver, DL);
    PG = std::make_shared<PathGen>(Solver, VG, DT, Backedges);
    return true;
}

PreservedAnalyses BugFreePass::run(Function &F, FunctionAnalysisManager &FAM) {
    if (!initialize(F, FAM))
        return PreservedAnalyses::all();
    return runOnFunction(F, FAM);
}

void BugFreePass::recalculate(Function &F) {
    DT->recalculate(F);
    PDT->recalculate(F);
    calculateBackedgesAndInLoopBlocks(F);
}

SMTExprRef
BugFreePass::computeBugFreeDelta(SmallVectorImpl<BugOnInst *> &Assertions) {
    SMTExprRef U = mkBVFalse(Solver);
    for (auto I : Assertions) {
        // queryWithAssertions() may mask some assertions.
        if (!I)
            continue;
        Value *V = I->getCondition();
        U = Solver->mkBVOr(U, VG->get(V));
    }
    return Solver->mkBVNot(U);
}

SMTExprRef BugFreePass::getBugFreeDelta(BasicBlock *BB) {
    Assertions.clear();
    // Ignore post-dominators if the option is set
    // or BB is in a loop.
    bool IgnorePostdom = IgnorePostOpt || isBlockInLoop.count(BB);
    Function *F = BB->getParent();
    for (auto i = F->begin(), e = F->end(); i != e; ++i) {
        BasicBlock *Blk = dyn_cast<BasicBlock>(i);
        // Collect blocks that (post)dominate BB: if BB is reachable,
        // these blocks must also be reachable, and we need to check
        // their bug assertions.
        if (!DT->dominates(Blk, BB)) {
            // Skip inspecting postdominators if BB is in a loop.
            if (IgnorePostdom)
                continue;
            if (!PDT->dominates(Blk, BB))
                continue;
        }
        for (auto i = Blk->begin(), e = Blk->end(); i != e; ++i) {
            if (BugOnInst *BOI = dyn_cast<BugOnInst>(i))
                Assertions.push_back(BOI);
        }
    }
    if (Assertions.empty())
        return nullptr;
    return computeBugFreeDelta(Assertions);
}

Optional<bool> BugFreePass::queryWithBugFreeDelta(SMTExprRef E,
                                                  SMTExprRef Delta) {
    SMTExprRef Q = Solver->mkBVAnd(E, Delta);
    Optional<bool> sat = queryBV(Solver, Q);
    if (!Buffer || !sat.hasValue() || sat.getValue())
        return sat;
    unsigned n = Assertions.size();
    // Compute the minimal bugon set.
    for (BugOnInst *&I : Assertions) {
        if (n <= 1)
            break;
        BugOnInst *Tmp = I;
        // Mask out this bugon and see if still unsat.
        I = nullptr;
        SMTExprRef Q = Solver->mkBVAnd(E, computeBugFreeDelta(Assertions));
        Optional<bool> sat = queryBV(Solver, Q);
        // Keep this assertions.
        if (!sat.hasValue() || sat.getValue())
            I = Tmp;
        else
            --n;
    }
    // Output the unsat core.
    BugOnInst **p = (BugOnInst **)Buffer;
    for (BugOnInst *I : Assertions) {
        if (!I)
            continue;
        *p++ = I;
    }
    *p = nullptr;
    return sat;
}

void BugFreePass::printMinimalAssertions() {
    if (!Buffer)
        return;
    int Count = 0;
    for (BugOnInst **p = (BugOnInst **)Buffer; *p; ++p)
        Count++;
    Diag << "ncore: " << Count << "\n";
    Diag << "core: \n";
    for (BugOnInst **p = (BugOnInst **)Buffer; *p; ++p) {
        BugOnInst *I = *p;
        Diag.location(I->getDebugLoc());
        Diag << "    - " << I->getAnnotation() << "\n";
    }
}
