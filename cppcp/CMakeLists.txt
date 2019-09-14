cmake_minimum_required (VERSION 3.10)
project(cppcp)

include(ClangFormat)


add_library(cppcp INTERFACE)
target_include_directories(cppcp INTERFACE ./)

clangformat_setup(cppcp.h)