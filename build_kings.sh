#!/bin/bash

# Variables
PROJECT_PATH="demo/kings.lpr"  # Path to the main project file
LAZARUS_BUILD_TOOL="/Applications/lazarus/lazbuild" # Path to Lazarus build tool
OUTPUT_DIR="build"           # Directory for build artifacts
STATIC_LIB="libkayte.a"      # Name for the static library

# Function to clean the build directory
function clean() {
  echo "Cleaning build directory..."
  rm -rf "$OUTPUT_DIR"
  mkdir -p "$OUTPUT_DIR"
  echo "Clean completed."
}

# Function to build the project
function build() {
  echo "Building Kings project..."
  if [ -f "$LAZARUS_BUILD_TOOL" ]; then
    "$LAZARUS_BUILD_TOOL" "$PROJECT_PATH"
    echo "Build completed successfully."
  else
    echo "Error: Lazarus build tool not found at $LAZARUS_BUILD_TOOL."
    exit 1
  fi
}

# Function to create the static library
function build_static_library() {
  echo "Creating static library..."
  OBJECT_FILES=$(find "$OUTPUT_DIR" -name "*.o") # Find all object files
  if [ -n "$OBJECT_FILES" ]; then
    ar rcs "$OUTPUT_DIR/$STATIC_LIB" $OBJECT_FILES
    echo "Static library created: $OUTPUT_DIR/$STATIC_LIB"
  else
    echo "Error: No object files found to create static library."
    exit 1
  fi
}

# Main script execution
case $1 in
  clean)
    clean
    ;;
  build)
    clean
    build
    ;;
  staticlib)
    build_static_library
    ;;
  all)
    clean
    build
    build_static_library
    ;;
  *)
    echo "Usage: $0 {clean|build|staticlib|all}"
    exit 1
    ;;
esac

