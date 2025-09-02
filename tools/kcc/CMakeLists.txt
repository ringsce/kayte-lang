cmake_minimum_required(VERSION 3.16 FATAL_ERROR)
project(KCC VERSION 1.10.0 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Compiler flags
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -Wunused-parameter")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra")

# Debug flags
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -g -O0 -DDEBUG -Wunused-parameter")
set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -O0 -DDEBUG")

# Release flags
set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O3 -DNDEBUG")
set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3 -DNDEBUG")

# Include directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Source files
set(KCC_SOURCES
        src/main.c
        src/lexer.c
        src/parser.c
        src/ast.c
        src/codegen.c
        src/error.c
        src/symbol_table.c
        src/preprocessor.c
)

# Header files
set(KCC_HEADERS
        include/kcc.h
        include/types.h
        include/lexer.h
        include/parser.h
        include/ast.h
        include/codegen.h
        include/error.h
        include/symbol_table.h
        include/preprocessor.h
)

# Create the executable
add_executable(kcc ${KCC_SOURCES} ${KCC_HEADERS})

# Link libraries (if needed)
# target_link_libraries(kcc ${LIBRARIES})

# Install target
install(TARGETS kcc DESTINATION bin)

# Optional: Create a test target
enable_testing()

# Test files
set(TEST_SOURCES
        tests/test_lexer.c
        tests/test_parser.c
        tests/test_main.c
)

add_executable(kcc_tests ${TEST_SOURCES}
        src/lexer.c
        src/parser.c
        src/ast.c
        src/error.c
        src/symbol_table.c
        src/preprocessor.c
)

target_include_directories(kcc_tests PRIVATE include)

add_test(NAME KCC_Tests COMMAND kcc_tests)

# Custom target for running tests
add_custom_target(test_all
        COMMAND ${CMAKE_CTEST_COMMAND} --verbose
        DEPENDS kcc_tests
)

# Print build configuration
message(STATUS "KCC C Compiler Configuration:")
message(STATUS "  Version: ${PROJECT_VERSION}")
message(STATUS "  Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "  C Compiler: ${CMAKE_C_COMPILER}")
message(STATUS "  CXX Compiler: ${CMAKE_CXX_COMPILER}")