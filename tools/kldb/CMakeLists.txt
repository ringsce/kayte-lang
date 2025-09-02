cmake_minimum_required(VERSION 3.16 FATAL_ERROR)
project(KayteDebugger C CXX)

#Set the C standard to C11
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED True)

#Define the source file for the executable
set(SOURCES kldb.c)

#Add the executable
add_executable(kldb ${SOURCES})

#You can uncomment the following line to add debug symbols for tools like gdb
set_target_properties(kldb PROPERTIES CMAKE_BUILD_TYPE "Debug")
