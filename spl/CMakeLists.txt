cmake_minimum_required(VERSION 3.24)
project(SkydownSpl VERSION 0.0.1 LANGUAGES CXX)

add_library(SkydownSpl INTERFACE)

target_include_directories(SkydownSpl
        INTERFACE
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
        )

add_library(Skydown::spl ALIAS SkydownSpl)

set(CMAKE_CXX_STANDARD 23)

add_executable(example example/main.cpp)
target_link_libraries(example PRIVATE Skydown::spl)
