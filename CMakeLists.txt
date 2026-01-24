cmake_minimum_required(VERSION 3.16 FATAL_ERROR)

project(Kayte VERSION 1.0.0 LANGUAGES C CXX NONE) 

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Set C standard
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

# Options
option(KAYTE_BUILD_FROM_SOURCE "Build Kayte IDE from source" OFF)
option(BUILD_TESTS "Build test suite" ON)
option(BUILD_EXAMPLES "Build example projects" ON)
option(ENABLE_WARNINGS "Enable compiler warnings" ON)

# Add cmake module path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# Find dependencies
message(STATUS "Finding dependencies...")

# Find Free Pascal Compiler
find_package(FPC REQUIRED)
if(FPC_FOUND)
    message(STATUS "Found FPC ${FPC_VERSION} at ${FPC_EXECUTABLE}")
    message(STATUS "  Target: ${FPC_TARGET_CPU}-${FPC_TARGET_OS}")
endif()

# Find Lazarus (optional)
find_package(Lazarus)
if(Lazarus_FOUND)
    message(STATUS "Found Lazarus ${Lazarus_VERSION} at ${Lazarus_EXECUTABLE}")
    if(LAZBUILD_EXECUTABLE)
        message(STATUS "  lazbuild: ${LAZBUILD_EXECUTABLE}")
    endif()
endif()

# Find or build Kayte IDE
find_package(KayteIDE)
if(KayteIDE_FOUND)
    message(STATUS "Found Kayte IDE at ${KayteIDE_EXECUTABLE}")
    if(KayteIDE_VERSION)
        message(STATUS "  Version: ${KayteIDE_VERSION}")
    endif()
else()
    message(STATUS "Kayte IDE not found. Set KAYTE_BUILD_FROM_SOURCE=ON to build from GitHub")
endif()

# Compiler flags
if(ENABLE_WARNINGS)
    if(CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
        add_compile_options(-Wall -Wextra -Wpedantic)
    elseif(MSVC)
        add_compile_options(/W4)
    endif()
endif()

# Output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Include directories
include_directories(
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_SOURCE_DIR}/src
)

if(KayteIDE_FOUND AND KayteIDE_INCLUDE_DIR)
    include_directories(${KayteIDE_INCLUDE_DIR})
endif()

# Source files for C++ components (if any)
set(KAYTE_SOURCES
    # Add your C++ source files here
    # src/main.cpp
    # src/kayte_runtime.cpp
)

set(KAYTE_HEADERS
    # Add your header files here
    # include/kayte_runtime.h
)

# Create library target (if you have C++ runtime/support code)
if(KAYTE_SOURCES)
    add_library(kayteruntime ${KAYTE_SOURCES} ${KAYTE_HEADERS})
    target_include_directories(kayteruntime PUBLIC
        $<BUILD_INTERFACE:${CMAKE_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
    )
endif()

# Pascal source files
set(PASCAL_SOURCES
    # Add your Pascal source files here
    # examples/hello.pas
    # examples/calculator.pas
)

# Build Pascal examples
if(BUILD_EXAMPLES AND FPC_FOUND)
    message(STATUS "Configuring Pascal examples...")
    
    # Example 1: Hello World
    if(EXISTS "${CMAKE_SOURCE_DIR}/examples/hello.pas")
        add_fpc_executable(hello_example
            SOURCES examples/hello.pas
            OUTPUT_NAME hello
            COMPILE_FLAGS -O2 -gl
        )
    endif()
    
    # Example 2: Add more examples as needed
    # add_fpc_executable(calculator_example
    #     SOURCES examples/calculator.pas
    #     OUTPUT_NAME calculator
    #     COMPILE_FLAGS -O2
    # )
endif()

# Build Lazarus projects
if(BUILD_EXAMPLES AND Lazarus_FOUND AND LAZBUILD_EXECUTABLE)
    message(STATUS "Configuring Lazarus projects...")
    
    # Example Lazarus project
    if(EXISTS "${CMAKE_SOURCE_DIR}/examples/gui_app.lpi")
        add_lazarus_project(gui_app_example
            PROJECT_FILE examples/gui_app.lpi
            BUILD_MODE Release
        )
    endif()
endif()

# Tests
if(BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()

# Installation
install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING PATTERN "*.h" PATTERN "*.hpp"
)

if(TARGET kayteruntime)
    install(TARGETS kayteruntime
        EXPORT KayteLangTargets
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
    )
endif()

# Install Pascal units (if any)
if(EXISTS "${CMAKE_SOURCE_DIR}/units")
    install(DIRECTORY units/
        DESTINATION share/kayte/units
        FILES_MATCHING PATTERN "*.pas" PATTERN "*.pp"
    )
endif()

# Export targets
install(EXPORT KayteLangTargets
    FILE KayteLangTargets.cmake
    NAMESPACE Kayte::
    DESTINATION lib/cmake/KayteLang
)

# Create config file
include(CMakePackageConfigHelpers)
configure_package_config_file(
    "${CMAKE_SOURCE_DIR}/cmake/KayteLangConfig.cmake.in"
    "${CMAKE_BINARY_DIR}/KayteLangConfig.cmake"
    INSTALL_DESTINATION lib/cmake/KayteLang
)

write_basic_package_version_file(
    "${CMAKE_BINARY_DIR}/KayteLangConfigVersion.cmake"
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
)

install(FILES
    "${CMAKE_BINARY_DIR}/KayteLangConfig.cmake"
    "${CMAKE_BINARY_DIR}/KayteLangConfigVersion.cmake"
    DESTINATION lib/cmake/KayteLang
)

# Install CMake modules
install(FILES
    cmake/FindKayteIDE.cmake
    cmake/FindFPC.cmake
    cmake/FindLazarus.cmake
    DESTINATION lib/cmake/KayteLang
)

# Summary
message(STATUS "")
message(STATUS "========== Configuration Summary ==========")
message(STATUS "Project: ${PROJECT_NAME} ${PROJECT_VERSION}")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "C++ Compiler: ${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}")
message(STATUS "Install prefix: ${CMAKE_INSTALL_PREFIX}")
message(STATUS "")
message(STATUS "Components:")
message(STATUS "  FPC: ${FPC_FOUND} ${FPC_VERSION}")
message(STATUS "  Lazarus: ${Lazarus_FOUND} ${Lazarus_VERSION}")
message(STATUS "  Kayte IDE: ${KayteIDE_FOUND} ${KayteIDE_VERSION}")
message(STATUS "")
message(STATUS "Options:")
message(STATUS "  Build tests: ${BUILD_TESTS}")
message(STATUS "  Build examples: ${BUILD_EXAMPLES}")
message(STATUS "  Build Kayte from source: ${KAYTE_BUILD_FROM_SOURCE}")
message(STATUS "==========================================")
message(STATUS "")