cmake_minimum_required(VERSION 3.14)
project(SplBenchmarks LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)

include(FetchContent)

# Fetch SPL library
FetchContent_Declare(
    skydown_dep
    SOURCE_DIR ${CMAKE_SOURCE_DIR}/../../
)
FetchContent_MakeAvailable(skydown_dep)

# Fetch Google Benchmark
FetchContent_Declare(
    googlebenchmark
    GIT_REPOSITORY https://github.com/google/benchmark.git
    GIT_TAG        v1.8.3
)
set(BENCHMARK_ENABLE_TESTING OFF CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googlebenchmark)

# Create benchmark executable
add_executable(run_benchmarks benchmarks.cpp)
target_link_libraries(run_benchmarks PRIVATE
    Skydown::spl
    benchmark::benchmark
    benchmark::benchmark_main
)
