cmake_minimum_required(VERSION 3.24)
project(SplPresentation LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)

# Pull in the Skydown::spl interface target from the repo root.
FetchContent_Declare(
        skydown_dep
        SOURCE_DIR ${CMAKE_SOURCE_DIR}/../../
)
FetchContent_MakeAvailable(skydown_dep)

add_executable(presentation presentation.cpp)
target_link_libraries(presentation PRIVATE Skydown::spl)
