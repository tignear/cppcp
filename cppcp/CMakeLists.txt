# CMakeList.txt : Parser2 の CMake プロジェクト。ソースを含めて、次を定義します:
# プロジェクト専用ロジックはこちらです。
#
cmake_minimum_required (VERSION 3.10)

add_library(cppcp INTERFACE)

target_include_directories(cppcp INTERFACE ./)