# FindFPC.cmake - Find Free Pascal Compiler
# This module finds the Free Pascal Compiler (fpc)
#
# Usage:
#   find_package(FPC)
#
# Variables defined by this module:
#   FPC_FOUND          - System has Free Pascal Compiler
#   FPC_EXECUTABLE     - The fpc compiler executable
#   FPC_VERSION        - The version of fpc found
#   FPC_VERSION_MAJOR  - Major version number
#   FPC_VERSION_MINOR  - Minor version number
#   FPC_VERSION_PATCH  - Patch version number

# Find the fpc executable
find_program(FPC_EXECUTABLE
    NAMES fpc
    PATHS
        # Linux paths
        /usr/bin
        /usr/local/bin
        /opt/fpc/bin
        # macOS paths
        /usr/local/bin
        /opt/homebrew/bin
        /opt/local/bin
        /Library/Developer/fpc/bin
        /usr/local/lib/fpc/bin
        # macOS Homebrew (Intel)
        /usr/local/Cellar/fpc/*/bin
        # macOS Homebrew (Apple Silicon)
        /opt/homebrew/Cellar/fpc/*/bin
        # macOS MacPorts
        /opt/local/lib/fpc/bin
        # Windows paths
        C:/FPC/bin/i386-win32
        C:/FPC/bin/x86_64-win64
        C:/lazarus/fpc/bin/i386-win32
        C:/lazarus/fpc/bin/x86_64-win64
    PATH_SUFFIXES
        bin
        i386-darwin
        x86_64-darwin
        aarch64-darwin
    DOC "Free Pascal Compiler"
)

# Get FPC version if found
if(FPC_EXECUTABLE)
    execute_process(
        COMMAND ${FPC_EXECUTABLE} -iV
        OUTPUT_VARIABLE FPC_VERSION
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    
    # Parse version components
    if(FPC_VERSION MATCHES "^([0-9]+)\\.([0-9]+)\\.([0-9]+)")
        set(FPC_VERSION_MAJOR ${CMAKE_MATCH_1})
        set(FPC_VERSION_MINOR ${CMAKE_MATCH_2})
        set(FPC_VERSION_PATCH ${CMAKE_MATCH_3})
    endif()
    
    # Get target CPU
    execute_process(
        COMMAND ${FPC_EXECUTABLE} -iTP
        OUTPUT_VARIABLE FPC_TARGET_CPU
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    
    # Get target OS
    execute_process(
        COMMAND ${FPC_EXECUTABLE} -iTO
        OUTPUT_VARIABLE FPC_TARGET_OS
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
endif()

# Handle standard find_package arguments
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(FPC
    REQUIRED_VARS FPC_EXECUTABLE
    VERSION_VAR FPC_VERSION
)

mark_as_advanced(FPC_EXECUTABLE)

# Function to compile Pascal source files
function(add_fpc_executable target)
    set(options)
    set(oneValueArgs OUTPUT_NAME)
    set(multiValueArgs SOURCES UNITS INCLUDE_DIRS COMPILE_FLAGS)
    cmake_parse_arguments(FPC "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
    
    if(NOT FPC_SOURCES)
        message(FATAL_ERROR "add_fpc_executable: SOURCES argument is required")
    endif()
    
    # Set output name
    if(NOT FPC_OUTPUT_NAME)
        set(FPC_OUTPUT_NAME ${target})
    endif()
    
    # Build include directories flags
    set(INCLUDE_FLAGS "")
    foreach(dir ${FPC_INCLUDE_DIRS})
        list(APPEND INCLUDE_FLAGS "-Fu${dir}")
    endforeach()
    
    # Build unit directories flags
    foreach(unit ${FPC_UNITS})
        list(APPEND INCLUDE_FLAGS "-Fu${unit}")
    endforeach()
    
    # Main source file (first in list)
    list(GET FPC_SOURCES 0 MAIN_SOURCE)
    
    # Output executable path
    set(OUTPUT_PATH "${CMAKE_CURRENT_BINARY_DIR}/${FPC_OUTPUT_NAME}${CMAKE_EXECUTABLE_SUFFIX}")
    
    # Create custom command to compile
    add_custom_command(
        OUTPUT ${OUTPUT_PATH}
        COMMAND ${FPC_EXECUTABLE}
            ${INCLUDE_FLAGS}
            ${FPC_COMPILE_FLAGS}
            -o${OUTPUT_PATH}
            ${CMAKE_CURRENT_SOURCE_DIR}/${MAIN_SOURCE}
        DEPENDS ${FPC_SOURCES}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Compiling Pascal executable ${FPC_OUTPUT_NAME}"
        VERBATIM
    )
    
    # Create custom target
    add_custom_target(${target} ALL DEPENDS ${OUTPUT_PATH})
    
    # Set target properties
    set_target_properties(${target} PROPERTIES
        OUTPUT_NAME ${FPC_OUTPUT_NAME}
    )
endfunction()