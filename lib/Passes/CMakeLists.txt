add_llvm_library(libStackPasses MODULE
    DebugLocHelper.cc
    Diagnostic.cc
    NullCheckElim.cc
    LoadElim.cc
    BugOn.cc
    BugOnInt.cc
    BugOnNull.cc
    BugOnUndef.cc
    SMTHelper.cc
    ValueGen.cc
    PathGen.cc
    BugFree.cc
    BugFreeAlgebraSimply.cc
    Passes.cc
    )

set_target_properties(libStackPasses PROPERTIES
  LIBRARY_OUTPUT_DIRECTORY ..
)
