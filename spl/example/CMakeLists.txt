cmake_minimum_required(VERSION 3.24)
project(SkydownSplExamples LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
include(FetchContent)


FetchContent_Declare(
        skydown_dep
        SOURCE_DIR ${CMAKE_SOURCE_DIR}/../../ # Points to the Skydown/ root
)
FetchContent_MakeAvailable(skydown_dep)

# Examples
add_executable(example_basics example_basics.cpp)
target_link_libraries(example_basics PRIVATE Skydown::spl)

add_executable(example_compose example_compose.cpp)
target_link_libraries(example_compose PRIVATE Skydown::spl)

add_executable(example_constexpr example_constexpr.cpp)
target_link_libraries(example_constexpr PRIVATE Skydown::spl)

add_executable(example_zip example_zip.cpp)
target_link_libraries(example_zip PRIVATE Skydown::spl)

add_executable(example_grouping example_grouping.cpp)
target_link_libraries(example_grouping PRIVATE Skydown::spl)

add_executable(example_flatten example_flatten.cpp)
target_link_libraries(example_flatten PRIVATE Skydown::spl)

add_executable(example_optional example_optional.cpp)
target_link_libraries(example_optional PRIVATE Skydown::spl)

add_executable(example_generators example_generators.cpp)
target_link_libraries(example_generators PRIVATE Skydown::spl)
