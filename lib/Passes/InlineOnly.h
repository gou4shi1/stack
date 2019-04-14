//===- InlineOnly.h - Inline Only pass --------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#pragma once

#include "llvm/Analysis/CGSCCPassManager.h"
#include "llvm/Analysis/InlineCost.h"
#include "llvm/Transforms/Utils/ImportedFunctionsInliningStatistics.h"

/// The inliner pass for the new pass manager.
///
/// This pass wires together the inlining utilities and the inline cost
/// analysis into a CGSCC pass. It considers every call in every function in
/// the SCC and tries to inline if profitable. It can be tuned with a number of
/// parameters to control what cost model is used and what tradeoffs are made
/// when making the decision.
///
/// It should be noted that the legacy inliners do considerably more than this
/// inliner pass does. They provide logic for manually merging allocas, and
/// doing considerable DCE including the DCE of dead functions. This pass makes
/// every attempt to be simpler. DCE of functions requires complex reasoning
/// about comdat groups, etc. Instead, it is expected that other more focused
/// passes be composed to achieve the same end result.
class InlineOnlyPass {
    llvm::InlineParams Params;
    std::unique_ptr<llvm::ImportedFunctionsInliningStatistics>
        ImportedFunctionsStats;

  public:
    static llvm::StringRef name() { return "inline-only"; }

    InlineOnlyPass(llvm::InlineParams Params = llvm::getInlineParams())
        : Params(std::move(Params)) {}
    ~InlineOnlyPass();
    InlineOnlyPass(InlineOnlyPass &&Arg)
        : Params(std::move(Arg.Params)),
          ImportedFunctionsStats(std::move(Arg.ImportedFunctionsStats)) {}

    llvm::PreservedAnalyses run(llvm::LazyCallGraph::SCC &C,
                                llvm::CGSCCAnalysisManager &AM,
                                llvm::LazyCallGraph &CG,
                                llvm::CGSCCUpdateResult &UR);
};
