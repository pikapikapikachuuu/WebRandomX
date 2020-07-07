cmake_minimum_required(VERSION 3.10.2)

project(softfloat)

aux_source_directory(. DIR_SRCS)

add_executable(tests ${DIR_SRCS})
set_property(TARGET tests PROPERTY POSITION_INDEPENDENT_CODE ON)
set_property(TARGET tests PROPERTY CXX_STANDARD 11)

set_target_properties(tests PROPERTIES LINK_FLAGS " -s WASM=1 -s ALLOW_MEMORY_GROWTH=1 -s SAFE_HEAP=1")
