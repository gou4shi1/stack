
add_llvm_library(libStackPasses MODULE
    BugOn.cc
    BugOnInt.cc
    BugOnNull.cc
    BugOnUndef.cc
    SMTHelper.cc
    ValueGen.cc
    PathGen.cc
    BugFree.cc
    Passes.cc
    )
