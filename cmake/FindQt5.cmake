# FindQt6.cmake
# Find Qt6 installation and components
#
# This module defines:
#  Qt6_FOUND - System has Qt6
#  Qt6_VERSION - The version of Qt6
#  Qt6_ROOT_DIR - The root directory of Qt6 installation
#  Qt6_INCLUDE_DIRS - Qt6 include directories
#  Qt6_LIBRARY_DIRS - Qt6 library directories
#  Qt6_BINARY_DIR - Qt6 binary directory (for tools)
#  Qt6_QMAKE_EXECUTABLE - Path to qmake
#  Qt6_MOC_EXECUTABLE - Path to moc (Meta-Object Compiler)
#  Qt6_UIC_EXECUTABLE - Path to uic (User Interface Compiler)
#  Qt6_RCC_EXECUTABLE - Path to rcc (Resource Compiler)
#  Qt6_LUPDATE_EXECUTABLE - Path to lupdate
#  Qt6_LRELEASE_EXECUTABLE - Path to lrelease
#
# Components can be specified: Core Widgets Gui Network Sql Xml etc.
# Example: find_package(Qt6 COMPONENTS Core Widgets REQUIRED)

# Allow user to specify Qt6 directory
set(Qt6_DIR_HINT "" CACHE PATH "Hint for Qt6 installation directory")

# Common Qt6 installation paths
set(Qt6_SEARCH_PATHS
    ${Qt6_DIR_HINT}
    $ENV{Qt6_DIR}
    $ENV{QTDIR}
    $ENV{QT6_DIR}
    $ENV{CMAKE_PREFIX_PATH}
    ~/Qt/6.*
    ~/Qt6
    /usr/local/Qt-6.*
    /usr/local/Qt6
    /opt/Qt/6.*
    /opt/Qt6
    "C:/Qt/6.*"
    "C:/Qt6"
    "$ENV{HOME}/Qt/6.*"
    "$ENV{USERPROFILE}/Qt/6.*"
)

# Try to find qmake first (most reliable way to find Qt)
find_program(Qt6_QMAKE_EXECUTABLE
    NAMES qmake6 qmake-qt6 qmake
    PATHS ${Qt6_SEARCH_PATHS}
    PATH_SUFFIXES
        bin
        gcc_64/bin
        clang_64/bin
        msvc2019_64/bin
        msvc2022_64/bin
        macos/bin
    DOC "Path to Qt6 qmake"
)

# Special handling for Homebrew on macOS
if(APPLE AND NOT Qt6_QMAKE_EXECUTABLE)
    execute_process(
        COMMAND brew --prefix qt@6
        OUTPUT_VARIABLE HOMEBREW_QT6_PREFIX
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    
    if(HOMEBREW_QT6_PREFIX)
        find_program(Qt6_QMAKE_EXECUTABLE
            NAMES qmake6 qmake
            PATHS ${HOMEBREW_QT6_PREFIX}/bin
            NO_DEFAULT_PATH
        )
        
        if(Qt6_QMAKE_EXECUTABLE)
            message(STATUS "Found Qt6 via Homebrew at ${HOMEBREW_QT6_PREFIX}")
        endif()
    endif()
endif()

if(Qt6_QMAKE_EXECUTABLE)
    # Get Qt6 version
    execute_process(
        COMMAND ${Qt6_QMAKE_EXECUTABLE} -query QT_VERSION
        OUTPUT_VARIABLE Qt6_VERSION
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    
    # Get Qt6 installation prefix
    execute_process(
        COMMAND ${Qt6_QMAKE_EXECUTABLE} -query QT_INSTALL_PREFIX
        OUTPUT_VARIABLE Qt6_ROOT_DIR
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    
    # Get include directory
    execute_process(
        COMMAND ${Qt6_QMAKE_EXECUTABLE} -query QT_INSTALL_HEADERS
        OUTPUT_VARIABLE Qt6_INCLUDE_DIRS
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    
    # Get library directory
    execute_process(
        COMMAND ${Qt6_QMAKE_EXECUTABLE} -query QT_INSTALL_LIBS
        OUTPUT_VARIABLE Qt6_LIBRARY_DIRS
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    
    # Get binary directory
    execute_process(
        COMMAND ${Qt6_QMAKE_EXECUTABLE} -query QT_INSTALL_BINS
        OUTPUT_VARIABLE Qt6_BINARY_DIR
        OUTPUT_STRIP_TRAILING_WHITESPACE
        ERROR_QUIET
    )
    
    # Find Qt6 tools
    find_program(Qt6_MOC_EXECUTABLE
        NAMES moc-qt6 moc
        PATHS ${Qt6_BINARY_DIR}
        NO_DEFAULT_PATH
    )
    
    find_program(Qt6_UIC_EXECUTABLE
        NAMES uic-qt6 uic
        PATHS ${Qt6_BINARY_DIR}
        NO_DEFAULT_PATH
    )
    
    find_program(Qt6_RCC_EXECUTABLE
        NAMES rcc-qt6 rcc
        PATHS ${Qt6_BINARY_DIR}
        NO_DEFAULT_PATH
    )
    
    find_program(Qt6_LUPDATE_EXECUTABLE
        NAMES lupdate-qt6 lupdate
        PATHS ${Qt6_BINARY_DIR}
        NO_DEFAULT_PATH
    )
    
    find_program(Qt6_LRELEASE_EXECUTABLE
        NAMES lrelease-qt6 lrelease
        PATHS ${Qt6_BINARY_DIR}
        NO_DEFAULT_PATH
    )
    
    # Set CMake prefix path for Qt6 modules
    list(APPEND CMAKE_PREFIX_PATH "${Qt6_ROOT_DIR}")
    
    # Find requested components
    if(Qt6_FIND_COMPONENTS)
        foreach(component ${Qt6_FIND_COMPONENTS})
            find_package(Qt6${component} QUIET PATHS ${Qt6_ROOT_DIR})
            if(Qt6${component}_FOUND)
                list(APPEND Qt6_LIBRARIES Qt6::${component})
                set(Qt6_${component}_FOUND TRUE)
            else()
                if(Qt6_FIND_REQUIRED_${component})
                    message(FATAL_ERROR "Qt6 component ${component} not found")
                endif()
            endif()
        endforeach()
    endif()
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Qt6
    REQUIRED_VARS Qt6_QMAKE_EXECUTABLE Qt6_ROOT_DIR
    VERSION_VAR Qt6_VERSION
)

mark_as_advanced(
    Qt6_QMAKE_EXECUTABLE
    Qt6_MOC_EXECUTABLE
    Qt6_UIC_EXECUTABLE
    Qt6_RCC_EXECUTABLE
    Qt6_LUPDATE_EXECUTABLE
    Qt6_LRELEASE_EXECUTABLE
    Qt6_ROOT_DIR
    Qt6_INCLUDE_DIRS
    Qt6_LIBRARY_DIRS
    Qt6_BINARY_DIR
)

# Print found information
if(Qt6_FOUND)
    message(STATUS "Found Qt6 ${Qt6_VERSION}")
    message(STATUS "  Qt6 root: ${Qt6_ROOT_DIR}")
    message(STATUS "  Qt6 includes: ${Qt6_INCLUDE_DIRS}")
    message(STATUS "  Qt6 libraries: ${Qt6_LIBRARY_DIRS}")
    message(STATUS "  Qt6 binaries: ${Qt6_BINARY_DIR}")
    
    if(Qt6_FIND_COMPONENTS)
        message(STATUS "  Qt6 components:")
        foreach(component ${Qt6_FIND_COMPONENTS})
            if(Qt6_${component}_FOUND)
                message(STATUS "    - ${component}: found")
            else()
                message(STATUS "    - ${component}: NOT found")
            endif()
        endforeach()
    endif()
endif()

# Helper function to enable automoc, autouic, autorcc
function(qt6_enable_auto_tools target)
    set_target_properties(${target} PROPERTIES
        AUTOMOC ON
        AUTOUIC ON
        AUTORCC ON
    )
endfunction()