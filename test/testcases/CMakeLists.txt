set(suffixes c cc cpp)
FOREACH(suffix IN LISTS suffixes)
    FILE(GLOB inFiles RELATIVE "${CMAKE_CURRENT_SOURCE_DIR}"
        "${CMAKE_CURRENT_SOURCE_DIR}/*.${suffix}")

    FOREACH(inFileName ${inFiles})
        STRING(REGEX REPLACE ".${suffix}\$" "" outFileName "${inFileName}")
        add_library(${outFileName} OBJECT ${inFileName})
    ENDFOREACH(inFileName)
ENDFOREACH(suffix)
