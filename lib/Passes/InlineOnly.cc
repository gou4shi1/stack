//===- InlineOnly.cpp - Inline Only Pass ----------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
// 
// Replace LLVM's InlinerPass with our InlineOnlyPass, which doesn't
// remove inlined functions for reducing false positives.
//
// 1) Skip reporting inlined code, based on debugging information.
// 2) Don't remove inlined functions to avoid missing bugs there.
//
//===----------------------------------------------------------------------===//

#include "InlineOnly.h"
#include "llvm/ADT/None.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Analysis/AssumptionCache.h"
#include "llvm/Analysis/BlockFrequencyInfo.h"
#include "llvm/Analysis/InlineCost.h"
#include "llvm/Analysis/LazyCallGraph.h"
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/ProfileSummaryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/IR/DiagnosticInfo.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/ImportedFunctionsInliningStatistics.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include <algorithm>
#include <cassert>
#include <functional>
#include <sstream>
#include <tuple>
#include <utility>
#include <vector>

using namespace llvm;

#define DEBUG_TYPE "inline-only"

STATISTIC(NumInlined, "Number of functions inlined");
STATISTIC(NumDeleted, "Number of functions deleted because all callers found");

// This weirdly named statistic tracks the number of times that, when attempting
// to inline a function A into B, we analyze the callers of B in order to see
// if those would be more profitable and blocked inline steps.
STATISTIC(NumCallerCallersAnalyzed, "Number of caller-callers analyzed");

namespace {

enum class InlineOnlyFunctionImportStatsOpts {
  No = 0,
  Basic = 1,
  Verbose = 2,
};

} // end anonymous namespace

static cl::opt<InlineOnlyFunctionImportStatsOpts> InlineOnlyFunctionImportStats(
    "inline-only-function-import-stats",
    cl::init(InlineOnlyFunctionImportStatsOpts::No),
    cl::values(clEnumValN(InlineOnlyFunctionImportStatsOpts::Basic, "basic",
                          "basic statistics"),
               clEnumValN(InlineOnlyFunctionImportStatsOpts::Verbose, "verbose",
                          "printing of statistics for each inlined function")),
    cl::Hidden, cl::desc("Enable inliner stats for imported functions"));

/// Flag to add inline messages as callsite attributes 'inline-remark'.
static cl::opt<bool>
    InlineOnlyRemarkAttribute("inline-only-remark-attribute", cl::init(false),
                          cl::Hidden,
                          cl::desc("Enable adding inline-remark attribute to"
                                   " callsites processed by inliner but decided"
                                   " to be not inlined"));

/// Return true if inlining of CS can block the caller from being
/// inlined which is proved to be more beneficial. \p IC is the
/// estimated inline cost associated with callsite \p CS.
/// \p TotalSecondaryCost will be set to the estimated cost of inlining the
/// caller if \p CS is suppressed for inlining.
static bool
shouldBeDeferred(Function *Caller, CallSite CS, InlineCost IC,
                 int &TotalSecondaryCost,
                 function_ref<InlineCost(CallSite CS)> GetInlineCost) {
  // For now we only handle local or inline functions.
  if (!Caller->hasLocalLinkage() && !Caller->hasLinkOnceODRLinkage())
    return false;
  // If the cost of inlining CS is non-positive, it is not going to prevent the
  // caller from being inlined into its callers and hence we don't need to
  // defer.
  if (IC.getCost() <= 0)
    return false;
  // Try to detect the case where the current inlining candidate caller (call
  // it B) is a static or linkonce-ODR function and is an inlining candidate
  // elsewhere, and the current candidate callee (call it C) is large enough
  // that inlining it into B would make B too big to inline later. In these
  // circumstances it may be best not to inline C into B, but to inline B into
  // its callers.
  //
  // This only applies to static and linkonce-ODR functions because those are
  // expected to be available for inlining in the translation units where they
  // are used. Thus we will always have the opportunity to make local inlining
  // decisions. Importantly the linkonce-ODR linkage covers inline functions
  // and templates in C++.
  //
  // FIXME: All of this logic should be sunk into getInlineCost. It relies on
  // the internal implementation of the inline cost metrics rather than
  // treating them as truly abstract units etc.
  TotalSecondaryCost = 0;
  // The candidate cost to be imposed upon the current function.
  int CandidateCost = IC.getCost() - 1;
  // If the caller has local linkage and can be inlined to all its callers, we
  // can apply a huge negative bonus to TotalSecondaryCost.
  bool ApplyLastCallBonus = Caller->hasLocalLinkage() && !Caller->hasOneUse();
  // This bool tracks what happens if we DO inline C into B.
  bool inliningPreventsSomeOuterInline = false;
  for (User *U : Caller->users()) {
    // If the caller will not be removed (either because it does not have a
    // local linkage or because the LastCallToStaticBonus has been already
    // applied), then we can exit the loop early.
    if (!ApplyLastCallBonus && TotalSecondaryCost >= IC.getCost())
      return false;
    CallSite CS2(U);

    // If this isn't a call to Caller (it could be some other sort
    // of reference) skip it.  Such references will prevent the caller
    // from being removed.
    if (!CS2 || CS2.getCalledFunction() != Caller) {
      ApplyLastCallBonus = false;
      continue;
    }

    InlineCost IC2 = GetInlineCost(CS2);
    ++NumCallerCallersAnalyzed;
    if (!IC2) {
      ApplyLastCallBonus = false;
      continue;
    }
    if (IC2.isAlways())
      continue;

    // See if inlining of the original callsite would erase the cost delta of
    // this callsite. We subtract off the penalty for the call instruction,
    // which we would be deleting.
    if (IC2.getCostDelta() <= CandidateCost) {
      inliningPreventsSomeOuterInline = true;
      TotalSecondaryCost += IC2.getCost();
    }
  }
  // If all outer calls to Caller would get inlined, the cost for the last
  // one is set very low by getInlineCost, in anticipation that Caller will
  // be removed entirely.  We did not account for this above unless there
  // is only one caller of Caller.
  if (ApplyLastCallBonus)
    TotalSecondaryCost -= InlineConstants::LastCallToStaticBonus;

  if (inliningPreventsSomeOuterInline && TotalSecondaryCost < IC.getCost())
    return true;

  return false;
}

static std::basic_ostream<char> &operator<<(std::basic_ostream<char> &R,
                                            const ore::NV &Arg) {
  return R << Arg.Val;
}

template <class RemarkT>
RemarkT &operator<<(RemarkT &&R, const InlineCost &IC) {
  using namespace ore;
  if (IC.isAlways()) {
    R << "(cost=always)";
  } else if (IC.isNever()) {
    R << "(cost=never)";
  } else {
    R << "(cost=" << ore::NV("Cost", IC.getCost())
      << ", threshold=" << ore::NV("Threshold", IC.getThreshold()) << ")";
  }
  if (const char *Reason = IC.getReason())
    R << ": " << ore::NV("Reason", Reason);
  return R;
}

static std::string inlineCostStr(const InlineCost &IC) {
  std::stringstream Remark;
  Remark << IC;
  return Remark.str();
}

/// Return the cost only if the inliner should attempt to inline at the given
/// CallSite. If we return the cost, we will emit an optimisation remark later
/// using that cost, so we won't do so from this function.
static Optional<InlineCost>
shouldInline(CallSite CS, function_ref<InlineCost(CallSite CS)> GetInlineCost,
             OptimizationRemarkEmitter &ORE) {
  using namespace ore;

  InlineCost IC = GetInlineCost(CS);
  Instruction *Call = CS.getInstruction();
  Function *Callee = CS.getCalledFunction();
  Function *Caller = CS.getCaller();

  if (IC.isAlways()) {
    LLVM_DEBUG(dbgs() << "    Inlining " << inlineCostStr(IC)
                      << ", Call: " << *CS.getInstruction() << "\n");
    return IC;
  }

  if (IC.isNever()) {
    LLVM_DEBUG(dbgs() << "    NOT Inlining " << inlineCostStr(IC)
                      << ", Call: " << *CS.getInstruction() << "\n");
    ORE.emit([&]() {
      return OptimizationRemarkMissed(DEBUG_TYPE, "NeverInline", Call)
             << NV("Callee", Callee) << " not inlined into "
             << NV("Caller", Caller) << " because it should never be inlined "
             << IC;
    });
    return IC;
  }

  if (!IC) {
    LLVM_DEBUG(dbgs() << "    NOT Inlining " << inlineCostStr(IC)
                      << ", Call: " << *CS.getInstruction() << "\n");
    ORE.emit([&]() {
      return OptimizationRemarkMissed(DEBUG_TYPE, "TooCostly", Call)
             << NV("Callee", Callee) << " not inlined into "
             << NV("Caller", Caller) << " because too costly to inline " << IC;
    });
    return IC;
  }

  int TotalSecondaryCost = 0;
  if (shouldBeDeferred(Caller, CS, IC, TotalSecondaryCost, GetInlineCost)) {
    LLVM_DEBUG(dbgs() << "    NOT Inlining: " << *CS.getInstruction()
                      << " Cost = " << IC.getCost()
                      << ", outer Cost = " << TotalSecondaryCost << '\n');
    ORE.emit([&]() {
      return OptimizationRemarkMissed(DEBUG_TYPE, "IncreaseCostInOtherContexts",
                                      Call)
             << "Not inlining. Cost of inlining " << NV("Callee", Callee)
             << " increases the cost of inlining " << NV("Caller", Caller)
             << " in other contexts";
    });

    // IC does not bool() to false, so get an InlineCost that will.
    // This will not be inspected to make an error message.
    return None;
  }

  LLVM_DEBUG(dbgs() << "    Inlining " << inlineCostStr(IC)
                    << ", Call: " << *CS.getInstruction() << '\n');
  return IC;
}

/// Return true if the specified inline history ID
/// indicates an inline history that includes the specified function.
static bool InlineHistoryIncludes(
    Function *F, int InlineHistoryID,
    const SmallVectorImpl<std::pair<Function *, int>> &InlineHistory) {
  while (InlineHistoryID != -1) {
    assert(unsigned(InlineHistoryID) < InlineHistory.size() &&
           "Invalid inline history ID");
    if (InlineHistory[InlineHistoryID].first == F)
      return true;
    InlineHistoryID = InlineHistory[InlineHistoryID].second;
  }
  return false;
}

static void emit_inlined_into(OptimizationRemarkEmitter &ORE, DebugLoc &DLoc,
                              const BasicBlock *Block, const Function &Callee,
                              const Function &Caller, const InlineCost &IC) {
  ORE.emit([&]() {
    bool AlwaysInline = IC.isAlways();
    StringRef RemarkName = AlwaysInline ? "AlwaysInline" : "Inlined";
    return OptimizationRemark(DEBUG_TYPE, RemarkName, DLoc, Block)
           << ore::NV("Callee", &Callee) << " inlined into "
           << ore::NV("Caller", &Caller) << " with " << IC;
  });
}

static void setInlineRemark(CallSite &CS, StringRef message) {
  if (!InlineOnlyRemarkAttribute)
    return;

  Attribute attr = Attribute::get(CS->getContext(), "inline-remark", message);
  CS.addAttribute(AttributeList::FunctionIndex, attr);
}

InlineOnlyPass::~InlineOnlyPass() {
  if (ImportedFunctionsStats) {
    assert(InlineOnlyFunctionImportStats != InlineOnlyFunctionImportStatsOpts::No);
    ImportedFunctionsStats->dump(InlineOnlyFunctionImportStats ==
                                 InlineOnlyFunctionImportStatsOpts::Verbose);
  }
}

PreservedAnalyses InlineOnlyPass::run(LazyCallGraph::SCC &InitialC,
                                   CGSCCAnalysisManager &AM, LazyCallGraph &CG,
                                   CGSCCUpdateResult &UR) {
  const ModuleAnalysisManager &MAM =
      AM.getResult<ModuleAnalysisManagerCGSCCProxy>(InitialC, CG).getManager();
  bool Changed = false;

  assert(InitialC.size() > 0 && "Cannot handle an empty SCC!");
  Module &M = *InitialC.begin()->getFunction().getParent();
  ProfileSummaryInfo *PSI = MAM.getCachedResult<ProfileSummaryAnalysis>(M);

  if (!ImportedFunctionsStats &&
      InlineOnlyFunctionImportStats != InlineOnlyFunctionImportStatsOpts::No) {
    ImportedFunctionsStats =
        llvm::make_unique<ImportedFunctionsInliningStatistics>();
    ImportedFunctionsStats->setModuleInfo(M);
  }

  // We use a single common worklist for calls across the entire SCC. We
  // process these in-order and append new calls introduced during inlining to
  // the end.
  //
  // Note that this particular order of processing is actually critical to
  // avoid very bad behaviors. Consider *highly connected* call graphs where
  // each function contains a small amonut of code and a couple of calls to
  // other functions. Because the LLVM inliner is fundamentally a bottom-up
  // inliner, it can handle gracefully the fact that these all appear to be
  // reasonable inlining candidates as it will flatten things until they become
  // too big to inline, and then move on and flatten another batch.
  //
  // However, when processing call edges *within* an SCC we cannot rely on this
  // bottom-up behavior. As a consequence, with heavily connected *SCCs* of
  // functions we can end up incrementally inlining N calls into each of
  // N functions because each incremental inlining decision looks good and we
  // don't have a topological ordering to prevent explosions.
  //
  // To compensate for this, we don't process transitive edges made immediate
  // by inlining until we've done one pass of inlining across the entire SCC.
  // Large, highly connected SCCs still lead to some amount of code bloat in
  // this model, but it is uniformly spread across all the functions in the SCC
  // and eventually they all become too large to inline, rather than
  // incrementally maknig a single function grow in a super linear fashion.
  SmallVector<std::pair<CallSite, int>, 16> Calls;

  FunctionAnalysisManager &FAM =
      AM.getResult<FunctionAnalysisManagerCGSCCProxy>(InitialC, CG)
          .getManager();

  // Populate the initial list of calls in this SCC.
  for (auto &N : InitialC) {
    auto &ORE =
        FAM.getResult<OptimizationRemarkEmitterAnalysis>(N.getFunction());
    // We want to generally process call sites top-down in order for
    // simplifications stemming from replacing the call with the returned value
    // after inlining to be visible to subsequent inlining decisions.
    // FIXME: Using instructions sequence is a really bad way to do this.
    // Instead we should do an actual RPO walk of the function body.
    for (Instruction &I : instructions(N.getFunction()))
      if (auto CS = CallSite(&I))
        if (Function *Callee = CS.getCalledFunction()) {
          if (!Callee->isDeclaration())
            Calls.push_back({CS, -1});
          else if (!isa<IntrinsicInst>(I)) {
            using namespace ore;
            setInlineRemark(CS, "unavailable definition");
            ORE.emit([&]() {
              return OptimizationRemarkMissed(DEBUG_TYPE, "NoDefinition", &I)
                     << NV("Callee", Callee) << " will not be inlined into "
                     << NV("Caller", CS.getCaller())
                     << " because its definition is unavailable"
                     << setIsVerbose();
            });
          }
        }
  }
  if (Calls.empty())
    return PreservedAnalyses::all();

  // Capture updatable variables for the current SCC and RefSCC.
  auto *C = &InitialC;
  auto *RC = &C->getOuterRefSCC();

  // When inlining a callee produces new call sites, we want to keep track of
  // the fact that they were inlined from the callee.  This allows us to avoid
  // infinite inlining in some obscure cases.  To represent this, we use an
  // index into the InlineHistory vector.
  SmallVector<std::pair<Function *, int>, 16> InlineHistory;

  // Track a set vector of inlined callees so that we can augment the caller
  // with all of their edges in the call graph before pruning out the ones that
  // got simplified away.
  SmallSetVector<Function *, 4> InlinedCallees;

  // Track the dead functions to delete once finished with inlining calls. We
  // defer deleting these to make it easier to handle the call graph updates.
  SmallVector<Function *, 4> DeadFunctions;

  // Loop forward over all of the calls. Note that we cannot cache the size as
  // inlining can introduce new calls that need to be processed.
  for (int i = 0; i < (int)Calls.size(); ++i) {
    // We expect the calls to typically be batched with sequences of calls that
    // have the same caller, so we first set up some shared infrastructure for
    // this caller. We also do any pruning we can at this layer on the caller
    // alone.
    Function &F = *Calls[i].first.getCaller();
    LazyCallGraph::Node &N = *CG.lookup(F);
    if (CG.lookupSCC(N) != C)
      continue;
    if (F.hasOptNone()) {
      setInlineRemark(Calls[i].first, "optnone attribute");
      continue;
    }

    LLVM_DEBUG(dbgs() << "Inlining calls in: " << F.getName() << "\n");

    // Get a FunctionAnalysisManager via a proxy for this particular node. We
    // do this each time we visit a node as the SCC may have changed and as
    // we're going to mutate this particular function we want to make sure the
    // proxy is in place to forward any invalidation events. We can use the
    // manager we get here for looking up results for functions other than this
    // node however because those functions aren't going to be mutated by this
    // pass.
    FunctionAnalysisManager &FAM =
        AM.getResult<FunctionAnalysisManagerCGSCCProxy>(*C, CG)
            .getManager();

    // Get the remarks emission analysis for the caller.
    auto &ORE = FAM.getResult<OptimizationRemarkEmitterAnalysis>(F);

    std::function<AssumptionCache &(Function &)> GetAssumptionCache =
        [&](Function &F) -> AssumptionCache & {
      return FAM.getResult<AssumptionAnalysis>(F);
    };
    auto GetBFI = [&](Function &F) -> BlockFrequencyInfo & {
      return FAM.getResult<BlockFrequencyAnalysis>(F);
    };

    auto GetInlineCost = [&](CallSite CS) {
      Function &Callee = *CS.getCalledFunction();
      auto &CalleeTTI = FAM.getResult<TargetIRAnalysis>(Callee);
      bool RemarksEnabled =
          Callee.getContext().getDiagHandlerPtr()->isMissedOptRemarkEnabled(
              DEBUG_TYPE);
      return getInlineCost(CS, Params, CalleeTTI, GetAssumptionCache, {GetBFI},
                           PSI, RemarksEnabled ? &ORE : nullptr);
    };

    // Now process as many calls as we have within this caller in the sequnece.
    // We bail out as soon as the caller has to change so we can update the
    // call graph and prepare the context of that new caller.
    bool DidInline = false;
    for (; i < (int)Calls.size() && Calls[i].first.getCaller() == &F; ++i) {
      int InlineHistoryID;
      CallSite CS;
      std::tie(CS, InlineHistoryID) = Calls[i];
      Function &Callee = *CS.getCalledFunction();

      if (InlineHistoryID != -1 &&
          InlineHistoryIncludes(&Callee, InlineHistoryID, InlineHistory)) {
        setInlineRemark(CS, "recursive");
        continue;
      }

      // Check if this inlining may repeat breaking an SCC apart that has
      // already been split once before. In that case, inlining here may
      // trigger infinite inlining, much like is prevented within the inliner
      // itself by the InlineHistory above, but spread across CGSCC iterations
      // and thus hidden from the full inline history.
      if (CG.lookupSCC(*CG.lookup(Callee)) == C &&
          UR.InlinedInternalEdges.count({&N, C})) {
        LLVM_DEBUG(dbgs() << "Skipping inlining internal SCC edge from a node "
                             "previously split out of this SCC by inlining: "
                          << F.getName() << " -> " << Callee.getName() << "\n");
        setInlineRemark(CS, "recursive SCC split");
        continue;
      }

      Optional<InlineCost> OIC = shouldInline(CS, GetInlineCost, ORE);
      // Check whether we want to inline this callsite.
      if (!OIC.hasValue()) {
        setInlineRemark(CS, "deferred");
        continue;
      }

      if (!OIC.getValue()) {
        // shouldInline() call returned a negative inline cost that explains
        // why this callsite should not be inlined.
        setInlineRemark(CS, inlineCostStr(*OIC));
        continue;
      }

      // Setup the data structure used to plumb customization into the
      // `InlineFunction` routine.
      InlineFunctionInfo IFI(
          /*cg=*/nullptr, &GetAssumptionCache, PSI,
          &FAM.getResult<BlockFrequencyAnalysis>(*(CS.getCaller())),
          &FAM.getResult<BlockFrequencyAnalysis>(Callee));

      // Get DebugLoc to report. CS will be invalid after Inliner.
      DebugLoc DLoc = CS->getDebugLoc();
      BasicBlock *Block = CS.getParent();

      using namespace ore;

      InlineResult IR = InlineFunction(CS, IFI);
      if (!IR) {
        setInlineRemark(CS, std::string(IR) + "; " + inlineCostStr(*OIC));
        ORE.emit([&]() {
          return OptimizationRemarkMissed(DEBUG_TYPE, "NotInlined", DLoc, Block)
                 << NV("Callee", &Callee) << " will not be inlined into "
                 << NV("Caller", &F) << ": " << NV("Reason", IR.message);
        });
        continue;
      }
      DidInline = true;
      InlinedCallees.insert(&Callee);

      ++NumInlined;

      emit_inlined_into(ORE, DLoc, Block, Callee, F, *OIC);

      // Add any new callsites to defined functions to the worklist.
      if (!IFI.InlinedCallSites.empty()) {
        int NewHistoryID = InlineHistory.size();
        InlineHistory.push_back({&Callee, InlineHistoryID});
        for (CallSite &CS : reverse(IFI.InlinedCallSites))
          if (Function *NewCallee = CS.getCalledFunction())
            if (!NewCallee->isDeclaration())
              Calls.push_back({CS, NewHistoryID});
      }

      if (InlineOnlyFunctionImportStats != InlineOnlyFunctionImportStatsOpts::No)
        ImportedFunctionsStats->recordInline(F, Callee);

      // Merge the attributes based on the inlining.
      AttributeFuncs::mergeAttributesForInlining(F, Callee);

      // For local functions, check whether this makes the callee trivially
      // dead. In that case, we can drop the body of the function eagerly
      // which may reduce the number of callers of other functions to one,
      // changing inline cost thresholds.
      if (false) {
      //if (Callee.hasLocalLinkage()) {
        // To check this we also need to nuke any dead constant uses (perhaps
        // made dead by this operation on other functions).
        Callee.removeDeadConstantUsers();
        if (Callee.use_empty() && !CG.isLibFunction(Callee)) {
          Calls.erase(
              std::remove_if(Calls.begin() + i + 1, Calls.end(),
                             [&Callee](const std::pair<CallSite, int> &Call) {
                               return Call.first.getCaller() == &Callee;
                             }),
              Calls.end());
          // Clear the body and queue the function itself for deletion when we
          // finish inlining and call graph updates.
          // Note that after this point, it is an error to do anything other
          // than use the callee's address or delete it.
          Callee.dropAllReferences();
          assert(find(DeadFunctions, &Callee) == DeadFunctions.end() &&
                 "Cannot put cause a function to become dead twice!");
          DeadFunctions.push_back(&Callee);
        }
      }
    }

    // Back the call index up by one to put us in a good position to go around
    // the outer loop.
    --i;

    if (!DidInline)
      continue;
    Changed = true;

    // Add all the inlined callees' edges as ref edges to the caller. These are
    // by definition trivial edges as we always have *some* transitive ref edge
    // chain. While in some cases these edges are direct calls inside the
    // callee, they have to be modeled in the inliner as reference edges as
    // there may be a reference edge anywhere along the chain from the current
    // caller to the callee that causes the whole thing to appear like
    // a (transitive) reference edge that will require promotion to a call edge
    // below.
    for (Function *InlinedCallee : InlinedCallees) {
      LazyCallGraph::Node &CalleeN = *CG.lookup(*InlinedCallee);
      for (LazyCallGraph::Edge &E : *CalleeN)
        RC->insertTrivialRefEdge(N, E.getNode());
    }

    // At this point, since we have made changes we have at least removed
    // a call instruction. However, in the process we do some incremental
    // simplification of the surrounding code. This simplification can
    // essentially do all of the same things as a function pass and we can
    // re-use the exact same logic for updating the call graph to reflect the
    // change.
    LazyCallGraph::SCC *OldC = C;
    C = &updateCGAndAnalysisManagerForFunctionPass(CG, *C, N, AM, UR);
    LLVM_DEBUG(dbgs() << "Updated inlining SCC: " << *C << "\n");
    RC = &C->getOuterRefSCC();

    // If this causes an SCC to split apart into multiple smaller SCCs, there
    // is a subtle risk we need to prepare for. Other transformations may
    // expose an "infinite inlining" opportunity later, and because of the SCC
    // mutation, we will revisit this function and potentially re-inline. If we
    // do, and that re-inlining also has the potentially to mutate the SCC
    // structure, the infinite inlining problem can manifest through infinite
    // SCC splits and merges. To avoid this, we capture the originating caller
    // node and the SCC containing the call edge. This is a slight over
    // approximation of the possible inlining decisions that must be avoided,
    // but is relatively efficient to store. We use C != OldC to know when
    // a new SCC is generated and the original SCC may be generated via merge
    // in later iterations.
    //
    // It is also possible that even if no new SCC is generated
    // (i.e., C == OldC), the original SCC could be split and then merged
    // into the same one as itself. and the original SCC will be added into
    // UR.CWorklist again, we want to catch such cases too.
    //
    // FIXME: This seems like a very heavyweight way of retaining the inline
    // history, we should look for a more efficient way of tracking it.
    if ((C != OldC || UR.CWorklist.count(OldC)) &&
        llvm::any_of(InlinedCallees, [&](Function *Callee) {
          return CG.lookupSCC(*CG.lookup(*Callee)) == OldC;
        })) {
      LLVM_DEBUG(dbgs() << "Inlined an internal call edge and split an SCC, "
                           "retaining this to avoid infinite inlining.\n");
      UR.InlinedInternalEdges.insert({&N, OldC});
    }
    InlinedCallees.clear();
  }

  // Now that we've finished inlining all of the calls across this SCC, delete
  // all of the trivially dead functions, updating the call graph and the CGSCC
  // pass manager in the process.
  //
  // Note that this walks a pointer set which has non-deterministic order but
  // that is OK as all we do is delete things and add pointers to unordered
  // sets.
  for (Function *DeadF : DeadFunctions) {
    // Get the necessary information out of the call graph and nuke the
    // function there. Also, cclear out any cached analyses.
    auto &DeadC = *CG.lookupSCC(*CG.lookup(*DeadF));
    FunctionAnalysisManager &FAM =
        AM.getResult<FunctionAnalysisManagerCGSCCProxy>(DeadC, CG)
            .getManager();
    FAM.clear(*DeadF, DeadF->getName());
    AM.clear(DeadC, DeadC.getName());
    auto &DeadRC = DeadC.getOuterRefSCC();
    CG.removeDeadFunction(*DeadF);

    // Mark the relevant parts of the call graph as invalid so we don't visit
    // them.
    UR.InvalidatedSCCs.insert(&DeadC);
    UR.InvalidatedRefSCCs.insert(&DeadRC);

    // And delete the actual function from the module.
    M.getFunctionList().erase(DeadF);
    ++NumDeleted;
  }

  if (!Changed)
    return PreservedAnalyses::all();

  // Even if we change the IR, we update the core CGSCC data structures and so
  // can preserve the proxy to the function analysis manager.
  PreservedAnalyses PA;
  PA.preserve<FunctionAnalysisManagerCGSCCProxy>();
  return PA;
}
