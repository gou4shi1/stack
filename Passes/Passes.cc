#include "BugOnInt.h"
#include "BugOnNull.h"
#include "BugOnUndef.h"
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/PassPlugin.h>

using namespace llvm;

extern "C" ::llvm::PassPluginLibraryInfo LLVM_ATTRIBUTE_WEAK
llvmGetPassPluginInfo() {
  return {
    LLVM_PLUGIN_API_VERSION, "BugOnPass", "v0.1",
    [](PassBuilder &PB) {
      PB.registerPipelineParsingCallback(
        [](StringRef PassName, FunctionPassManager &FPM,
           ArrayRef<PassBuilder::PipelineElement>) {
          if(PassName == "bugon"){
            FPM.addPass(BugOnIntPass());
            FPM.addPass(BugOnNullPass());
            FPM.addPass(BugOnUndefPass());
            return true;
          }
          return false;
        }
      );
    }
  };
}

