#!/bin/bash

# Define project paths
LAZBUILD="/Applications/lazarus/lazbuild"
PROJECT_FILE="demo/kings.lpr"
BUILD_DIR="demo/"
EXECUTABLE_NAME="kings"

# Function to clean the build directory
clean() {
  echo "Cleaning build directory..."
  if [ -d "$BUILD_DIR" ]; then
    rm -rf "$BUILD_DIR"
    echo "Build directory cleaned."
  else
    echo "No build directory to clean."
  fi
}

# Function to build the project
build() {
  echo "Building $PROJECT_FILE..."
  if [ -f "$LAZBUILD" ]; then
    mkdir -p "$BUILD_DIR"
    $LAZBUILD --bm=Release --output="$BUILD_DIR/$EXECUTABLE_NAME" "$PROJECT_FILE"
    if [ $? -eq 0 ]; then
      echo "Build completed successfully."
    else
      echo "Build failed."
    fi
  else
    echo "Error: lazbuild not found at $LAZBUILD."
    exit 1
  fi
}

# Main menu
case "$1" in
  build)
    build
    ;;
  clean)
    clean
    ;;
  rebuild)
    clean
    build
    ;;
  *)
    echo "Usage: $0 {build|clean|rebuild}"
    exit 1
    ;;
esac

