cmake_minimum_required (VERSION 3.10)

include(${PROJECT_SOURCE_DIR}/cmake/FormatFilesWithClangFormat.cmake)


add_library(cppcp INTERFACE)
target_include_directories(cppcp INTERFACE ./)
# clang_format(${target})