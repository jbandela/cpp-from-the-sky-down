cmake_minimum_required(VERSION 3.14)
project(AwesomeLibraryTests LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
# Create the test executable
add_executable(run_tests tests.cpp)

# Link against the library target (defined in the top-level file)
# and GTest (also defined in the top-level file).
target_link_libraries(run_tests PRIVATE
        Skydown::spl
        GTest::gmock
        GTest::gmock_main
        )

include(GoogleTest)
gtest_add_tests(TARGET run_tests)