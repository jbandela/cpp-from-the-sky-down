cmake_minimum_required(VERSION 3.24)
project(SkydownSplExample LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)

add_executable(example main.cpp)
target_link_libraries(example PRIVATE Skydown::spl)
