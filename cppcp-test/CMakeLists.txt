cmake_minimum_required(VERSION 3.10)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
include(${PROJECT_SOURCE_DIR}/cmake/DownloadProject/DownloadProject.cmake)
download_project(PROJ googletest
                GIT_REPOSITORY https://github.com/google/googletest.git
                GIT_TAG master
                UPDATE_DISCONNECTED 1
)

#add_subdirectory(${googletest_SOURCE_DIR} ${googletest_BINARY_DIR})

add_executable(cppcpTest "cppcp-test.cpp")

target_link_libraries(cppcpTest gtest_main cppcp)
include_directories(${PROJECT_SOURCE_DIR}/src)

include(GoogleTest)

add_test(NAME cppcpTest COMMAND cppcpTest)