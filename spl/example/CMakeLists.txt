cmake_minimum_required(VERSION 3.24)
project(SkydownSplExample LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
include(FetchContent)


FetchContent_Declare(
        skydown_dep
        SOURCE_DIR ${CMAKE_SOURCE_DIR}/../../ # Points to the Skydown/ root
)
FetchContent_MakeAvailable(skydown_dep)


add_executable(example main.cpp)
target_link_libraries(example PRIVATE Skydown::spl)
