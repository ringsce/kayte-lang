# FindKayteIDE.cmake
# Find or build the Kayte IDE installation
#
# This module defines:
#  KayteIDE_FOUND - System has Kayte IDE
#  KayteIDE_EXECUTABLE - The Kayte IDE executable
#  KayteIDE_VERSION - The version of Kayte IDE
#  KayteIDE_ROOT_DIR - The root directory of Kayte IDE installation
#  KayteIDE_INCLUDE_DIR - Include directories for Kayte
#  KayteIDE_LIBRARY_DIR - Library directories for Kayte
#  KayteIDE_SOURCE_DIR - Source directory if built from GitHub
#
# Options:
#  KAYTE_BUILD_FROM_SOURCE - If TRUE, clone and build from GitHub
#  KAYTE_GIT_REPOSITORY - Git repository URL (default: https://github.com/ringsce/kayteide.git)
#  KAYTE_GIT_TAG - Git tag/branch to checkout (default: main)

# Configuration options
option(KAYTE_BUILD_FROM_SOURCE "Build Kayte IDE from source" OFF)
set(KAYTE_GIT_REPOSITORY "https://github.com/ringsce/kayteide.git" CACHE STRING "Kayte IDE git repository")
set(KAYTE_GIT_TAG "main" CACHE STRING "Kayte IDE git tag or branch")

# Try to find existing installation first
find_program(KayteIDE_EXECUTABLE
    NAMES KayteIDE kayte kayte-ide kayteide KayteIDE.exe kayte.exe
    PATHS
        "$ENV{KAYTE_HOME}/bin"
        "$ENV{KAYTE_IDE_HOME}/bin"
        "${CMAKE_BINARY_DIR}/kayteide-build/bin"
        /usr/local/kayte/bin
        /usr/kayte/bin
        /opt/kayte/bin
        "C:/Program Files/Kayte/bin"
        "C:/Program Files (x86)/Kayte/bin"
        "$ENV{ProgramFiles}/Kayte/bin"
        "$ENV{ProgramFiles(x86)}/Kayte/bin"
        ~/kayte/bin
    DOC "Path to the Kayte IDE executable"
)

# If not found and build from source is requested, clone and build
if(NOT KayteIDE_EXECUTABLE AND KAYTE_BUILD_FROM_SOURCE)
    find_package(Git REQUIRED)
    
    set(KayteIDE_SOURCE_DIR "${CMAKE_BINARY_DIR}/kayteide-src")
    set(KayteIDE_BUILD_DIR "${CMAKE_BINARY_DIR}/kayteide-build")
    
    message(STATUS "Kayte IDE not found. Cloning from ${KAYTE_GIT_REPOSITORY}...")
    
    # Clone the repository if not already cloned
    if(NOT EXISTS "${KayteIDE_SOURCE_DIR}")
        execute_process(
            COMMAND ${GIT_EXECUTABLE} clone ${KAYTE_GIT_REPOSITORY} ${KayteIDE_SOURCE_DIR}
            RESULT_VARIABLE GIT_CLONE_RESULT
            OUTPUT_VARIABLE GIT_CLONE_OUTPUT
            ERROR_VARIABLE GIT_CLONE_ERROR
        )
        
        if(NOT GIT_CLONE_RESULT EQUAL 0)
            message(FATAL_ERROR "Failed to clone Kayte IDE: ${GIT_CLONE_ERROR}")
        endif()
    endif()
    
    # Checkout the specified tag/branch
    execute_process(
        COMMAND ${GIT_EXECUTABLE} checkout ${KAYTE_GIT_TAG}
        WORKING_DIRECTORY ${KayteIDE_SOURCE_DIR}
        RESULT_VARIABLE GIT_CHECKOUT_RESULT
        OUTPUT_QUIET
        ERROR_QUIET
    )
    
    # Find Qt6 (required for building KayteIDE)
    find_package(Qt6 COMPONENTS Core Widgets QUIET)
    if(NOT Qt6_FOUND)
        message(WARNING "Qt6 not found. KayteIDE requires Qt6 to build. Please install Qt6 and try again.")
        message(WARNING "  macOS: brew install qt@6")
        message(WARNING "  Ubuntu/Debian: sudo apt install qt6-base-dev")
        message(WARNING "  Fedora: sudo dnf install qt6-qtbase-devel")
    else()
        message(STATUS "Building Kayte IDE...")
        
        # Configure with CMake
        execute_process(
            COMMAND ${CMAKE_COMMAND} 
                -S ${KayteIDE_SOURCE_DIR}
                -B ${KayteIDE_BUILD_DIR}
                -DCMAKE_BUILD_TYPE=Release
            RESULT_VARIABLE CMAKE_CONFIG_RESULT
            OUTPUT_VARIABLE CMAKE_CONFIG_OUTPUT
            ERROR_VARIABLE CMAKE_CONFIG_ERROR
        )
        
        if(NOT CMAKE_CONFIG_RESULT EQUAL 0)
            message(FATAL_ERROR "Failed to configure Kayte IDE: ${CMAKE_CONFIG_ERROR}")
        endif()
        
        # Build
        execute_process(
            COMMAND ${CMAKE_COMMAND} --build ${KayteIDE_BUILD_DIR} --parallel
            RESULT_VARIABLE CMAKE_BUILD_RESULT
            OUTPUT_VARIABLE CMAKE_BUILD_OUTPUT
            ERROR_VARIABLE CMAKE_BUILD_ERROR
        )
        
        if(NOT CMAKE_BUILD_RESULT EQUAL 0)
            message(FATAL_ERROR "Failed to build Kayte IDE: ${CMAKE_BUILD_ERROR}")
        endif()
        
        message(STATUS "Kayte IDE built successfully at ${KayteIDE_BUILD_DIR}")
        
        # Try to find the executable again
        find_program(KayteIDE_EXECUTABLE
            NAMES KayteIDE kayte-ide kayteide KayteIDE.exe
            PATHS ${KayteIDE_BUILD_DIR}/bin
            NO_DEFAULT_PATH
        )
    endif()
endif()

# Process found executable
if(KayteIDE_EXECUTABLE)
    # Get the installation root directory
    get_filename_component(KayteIDE_BIN_DIR "${KayteIDE_EXECUTABLE}" DIRECTORY)
    get_filename_component(KayteIDE_ROOT_DIR "${KayteIDE_BIN_DIR}" DIRECTORY)
    
    # Try to determine version
    execute_process(
        COMMAND "${KayteIDE_EXECUTABLE}" --version
        OUTPUT_VARIABLE KayteIDE_VERSION_OUTPUT
        ERROR_VARIABLE KayteIDE_VERSION_ERROR
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    
    if(KayteIDE_VERSION_OUTPUT)
        string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+" KayteIDE_VERSION "${KayteIDE_VERSION_OUTPUT}")
    else()
        # Try to get version from git if built from source
        if(EXISTS "${KayteIDE_SOURCE_DIR}/.git")
            execute_process(
                COMMAND ${GIT_EXECUTABLE} describe --tags --always
                WORKING_DIRECTORY ${KayteIDE_SOURCE_DIR}
                OUTPUT_VARIABLE KayteIDE_VERSION
                OUTPUT_STRIP_TRAILING_WHITESPACE
                ERROR_QUIET
            )
        endif()
    endif()
    
    # Set include and library directories
    if(KAYTE_BUILD_FROM_SOURCE AND EXISTS "${KayteIDE_SOURCE_DIR}")
        set(KayteIDE_INCLUDE_DIR "${KayteIDE_SOURCE_DIR}")
        set(KayteIDE_LIBRARY_DIR "${KayteIDE_BUILD_DIR}/lib")
    else()
        set(KayteIDE_INCLUDE_DIR "${KayteIDE_ROOT_DIR}/include")
        set(KayteIDE_LIBRARY_DIR "${KayteIDE_ROOT_DIR}/lib")
        
        # Check for common Kayte directories
        if(NOT EXISTS "${KayteIDE_INCLUDE_DIR}")
            set(KayteIDE_INCLUDE_DIR "${KayteIDE_ROOT_DIR}/src")
        endif()
    endif()
    
    message(STATUS "Found Kayte IDE: ${KayteIDE_EXECUTABLE}")
    if(KayteIDE_VERSION)
        message(STATUS "Kayte IDE version: ${KayteIDE_VERSION}")
    endif()
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(KayteIDE
    REQUIRED_VARS KayteIDE_EXECUTABLE KayteIDE_ROOT_DIR
    VERSION_VAR KayteIDE_VERSION
)

mark_as_advanced(
    KayteIDE_EXECUTABLE
    KayteIDE_ROOT_DIR
    KayteIDE_INCLUDE_DIR
    KayteIDE_LIBRARY_DIR
    KayteIDE_VERSION
    KayteIDE_SOURCE_DIR
    KayteIDE_BUILD_DIR
)

# Create imported target
if(KayteIDE_FOUND AND NOT TARGET Kayte::IDE)
    add_executable(Kayte::IDE IMPORTED)
    set_target_properties(Kayte::IDE PROPERTIES
        IMPORTED_LOCATION "${KayteIDE_EXECUTABLE}"
    )
endif()