# CMakeList.txt : 最上位の CMake プロジェクト ファイル。グローバル構成を行います
# また、サブプロジェクトをここに含めます。
#
cmake_minimum_required (VERSION 3.10)
enable_testing()
set(CMAKE_CXX_STANDARD 17) 
set(CMAKE_CXX_STANDARD_REQUIRED ON) 
set(CMAKE_CXX_EXTENSIONS OFF) 
#option(CMAKE_USE_WIN32_THREADS_INIT "using WIN32 threads" ON)
#option(gtest_disable_pthreads "Disable uses of pthreads in gtest." ON)


project ("cppcp-distribution")

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/DownloadProject)
list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake/clangformat-cmake/cmake)

include(${PROJECT_SOURCE_DIR}/cmake/clangformat-cmake/cmake/ClangFormat.cmake)



clangformat_setup()
# サブプロジェクトを含めます。
add_subdirectory("cppcp")
add_subdirectory("cppcp-test")