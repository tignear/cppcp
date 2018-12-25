# CMakeList.txt : Parser2 の CMake プロジェクト。ソースを含めて、次を定義します:
# プロジェクト専用ロジックはこちらです。
#
cmake_minimum_required (VERSION 3.8)

# ソースをこのプロジェクトの実行可能ファイルに追加します。
add_executable (Parser2 "Parser2.cpp" "Parser2.h")

# TODO: テストを追加し、必要な場合は、ターゲットをインストールします。
