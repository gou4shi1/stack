# remember the output file for dependencies
SET(outFiles)

# add command to generate IR of *.c
FILE(GLOB inFiles RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.c")

FOREACH(inFileName ${inFiles})
    STRING(REGEX REPLACE ".c\$" ".ll" outFileName "${inFileName}")
    SET(outFile "${CMAKE_CURRENT_BINARY_DIR}/${outFileName}")
    SET(inFile "${CMAKE_CURRENT_SOURCE_DIR}/${inFileName}")

    ADD_CUSTOM_COMMAND(OUTPUT "${outFile}"
        COMMAND clang -g -emit-llvm -S "${inFile}"
        DEPENDS "${inFile}"
        COMMENT "Generating IR of ${inFile}")

    SET(outFiles ${outFiles} "${outFile}")
ENDFOREACH(inFileName)

# add command to generate IR of *.cc
FILE(GLOB inFiles RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
    "${CMAKE_CURRENT_SOURCE_DIR}/*.cc")

FOREACH(inFileName ${inFiles})
    STRING(REGEX REPLACE ".cc\$" ".ll" outFileName "${inFileName}")
    SET(outFile "${CMAKE_CURRENT_BINARY_DIR}/${outFileName}")
    SET(inFile "${CMAKE_CURRENT_SOURCE_DIR}/${inFileName}")

    ADD_CUSTOM_COMMAND(OUTPUT "${outFile}"
        COMMAND clang++ -g -emit-llvm -S "${inFile}"
        DEPENDS "${inFile}"
        COMMENT "Generating IR of ${inFile}")

    SET(outFiles ${outFiles} "${outFile}")
ENDFOREACH(inFileName)

# Setup a target to drive the generation
ADD_CUSTOM_TARGET(testcases DEPENDS ${outFiles})
