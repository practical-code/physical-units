cmake_minimum_required(VERSION 3.2)

set(TARGET physical-units)

project(${TARGET} LANGUAGES CXX VERSION 1)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(
    ${TARGET}
    "${PROJECT_SOURCE_DIR}/src/PhysicalUnits.cpp"
)

target_include_directories(
    ${TARGET}
    PRIVATE
    "${PROJECT_SOURCE_DIR}/include"
)

target_compile_options(${TARGET} PRIVATE -Wall -Wextra -O3 -pedantic-errors -fmax-errors=1)
