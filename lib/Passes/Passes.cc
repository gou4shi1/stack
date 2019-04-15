#include "BugFreeAlgebraSimply.h"
#include "BugOnInt.h"
#include "BugOnNull.h"
#include "BugOnUndef.h"
#include "InlineOnly.h"
#include "LoadElim.h"
#include "NullCheckElim.h"
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>

using namespace llvm;

extern "C" ::llvm::PassPluginLibraryInfo LLVM_ATTRIBUTE_WEAK
llvmGetPassPluginInfo() {
    return {LLVM_PLUGIN_API_VERSION, "StackPass", "v0.1", [](PassBuilder &PB) {
        PB.registerPipelineParsingCallback([](StringRef PassName, CGSCCPassManager &SCCPM, ArrayRef<PassBuilder::PipelineElement>) {
            if (PassName == "inline-only") {
                SCCPM.addPass(InlineOnlyPass());
                return true;
            }
            return false;
            });
        PB.registerPipelineParsingCallback([](StringRef PassName, FunctionPassManager &FPM, ArrayRef<PassBuilder::PipelineElement>) {
            if (PassName == "elim") {
                FPM.addPass(NullCheckElimPass());
                FPM.addPass(LoadElimPass());
                return true;
            }
            if (PassName == "bugon") {
                FPM.addPass(BugOnIntPass());
                FPM.addPass(BugOnNullPass());
                FPM.addPass(BugOnUndefPass());
                return true;
            }
            if (PassName == "bugfree") {
                FPM.addPass(BugFreeAlgebraSimplyPass());
                return true;
            }
            return false;
            });
        }
    };
}
