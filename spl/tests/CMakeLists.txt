cmake_minimum_required(VERSION 3.14)
project(AwesomeLibraryTests LANGUAGES CXX)
include(FetchContent)

set(CMAKE_CXX_STANDARD 23)
# Create the test executable
add_executable(run_tests tests.cpp)

FetchContent_Declare(
        skydown_dep
        SOURCE_DIR ${CMAKE_SOURCE_DIR}/../../ # Points to the Skydown/ root
)
FetchContent_MakeAvailable(skydown_dep)

enable_testing()

include(FetchContent)
FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG        v1.17.0
)
FetchContent_MakeAvailable(googletest)



# Link against the library target (defined in the top-level file)
# and GTest (also defined in the top-level file).
target_link_libraries(run_tests PRIVATE
        Skydown::spl
        GTest::gmock
        GTest::gmock_main
        )

include(GoogleTest)
gtest_add_tests(TARGET run_tests)