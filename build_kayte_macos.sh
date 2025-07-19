#!/bin/bash
#
# build_kayte_macos.sh
#
# This script compiles the Kayte language project for macOS,
# automatically detecting the architecture (x86_64 or arm64)
# and producing universal (fat) binaries.
#
# Dependencies:
# - Xcode Command Line Tools (clang, ar, lipo)
# - kayte_runtime.c
# - kayte_program_x86_64.ll
# - kayte_program_arm64.ll

# Exit immediately if a command exits with a non-zero status.
set -e

# --- Configuration ---
RUNTIME_C_FILE="kayte_runtime.c"
LL_X86_64_FILE="kayte_program_x86_64.ll"
LL_ARM64_FILE="kayte_program_arm64.ll"

OUTPUT_DIR="build_macos"
EXECUTABLE_NAME="kayte_app"
STATIC_LIB_NAME="libkayte_universal.a"
DYNAMIC_LIB_NAME="libkayte_universal.dylib"

# --- Functions ---

log_step() {
    echo "--- $1 ---"
}

check_command() {
    if ! command -v "$1" &> /dev/null; then
        echo "Error: '$1' command not found. Please ensure Xcode Command Line Tools are installed."
        echo "Run: xcode-select --install"
        exit 1
    fi
}

# --- Main Script ---

log_step "Checking prerequisites..."
check_command "clang"
check_command "ar"
check_command "lipo"

if [[ ! -f "$RUNTIME_C_FILE" ]]; then
    echo "Error: $RUNTIME_C_FILE not found in the current directory."
    exit 1
fi
if [[ ! -f "$LL_X86_64_FILE" ]]; then
    echo "Error: $LL_X86_64_FILE not found in the current directory."
    exit 1
fi
if [[ ! -f "$LL_ARM64_FILE" ]]; then
    echo "Error: $LL_ARM64_FILE not found in the current directory."
    exit 1
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"
echo "Output directory: $OUTPUT_DIR"

log_step "Compiling Kayte Runtime ($RUNTIME_C_FILE) for x86_64 and arm64..."
clang -c "$RUNTIME_C_FILE" -target x86_64-apple-macosx14.0.0 -o "$OUTPUT_DIR/kayte_runtime_x86_64.o"
clang -c "$RUNTIME_C_FILE" -target arm64-apple-macosx14.0.0 -o "$OUTPUT_DIR/kayte_runtime_arm64.o"
echo "Runtime compiled for both architectures."

log_step "Compiling Kayte LLVM IR (.ll files) for x86_64 and arm64..."
clang -c "$LL_X86_64_FILE" -o "$OUTPUT_DIR/kayte_program_x86_64.o"
clang -c "$LL_ARM64_FILE" -o "$OUTPUT_DIR/kayte_program_arm64.o"
echo "Kayte program compiled for both architectures."

log_step "Creating Universal Executable ($EXECUTABLE_NAME)..."
clang "$OUTPUT_DIR/kayte_program_x86_64.o" "$OUTPUT_DIR/kayte_runtime_x86_64.o" \
      "$OUTPUT_DIR/kayte_program_arm64.o" "$OUTPUT_DIR/kayte_runtime_arm64.o" \
      -o "$OUTPUT_DIR/$EXECUTABLE_NAME" \
      -lSystem -lc # Link with standard system libraries
echo "Universal executable created: $OUTPUT_DIR/$EXECUTABLE_NAME"
echo "You can run it with: $OUTPUT_DIR/$EXECUTABLE_NAME"

log_step "Creating Universal Static Library ($STATIC_LIB_NAME)..."
# Create individual static libs first
ar rcs "$OUTPUT_DIR/libkayte_x86_64.a" "$OUTPUT_DIR/kayte_program_x86_64.o" "$OUTPUT_DIR/kayte_runtime_x86_64.o"
ar rcs "$OUTPUT_DIR/libkayte_arm64.a" "$OUTPUT_DIR/kayte_program_arm64.o" "$OUTPUT_DIR/kayte_runtime_arm64.o"
# Lipo them into a universal static library
lipo -create "$OUTPUT_DIR/libkayte_x86_64.a" "$OUTPUT_DIR/libkayte_arm64.a" -output "$OUTPUT_DIR/$STATIC_LIB_NAME"
echo "Universal static library created: $OUTPUT_DIR/$STATIC_LIB_NAME"

log_step "Creating Universal Dynamic Library ($DYNAMIC_LIB_NAME)..."
# Create individual dynamic libs first
clang -shared -o "$OUTPUT_DIR/libkayte_x86_64.dylib" "$OUTPUT_DIR/kayte_program_x86_64.o" "$OUTPUT_DIR/kayte_runtime_x86_64.o" -lSystem -lc
clang -shared -o "$OUTPUT_DIR/libkayte_arm64.dylib" "$OUTPUT_DIR/kayte_program_arm64.o" "$OUTPUT_DIR/kayte_runtime_arm64.o" -lSystem -lc
# Lipo them into a universal dynamic library
lipo -create "$OUTPUT_DIR/libkayte_x86_64.dylib" "$OUTPUT_DIR/libkayte_arm64.dylib" -output "$OUTPUT_DIR/$DYNAMIC_LIB_NAME"
echo "Universal dynamic library created: $OUTPUT_DIR/$DYNAMIC_LIB_NAME"

log_step "Cleaning up intermediate object files..."
rm "$OUTPUT_DIR"/*.o "$OUTPUT_DIR"/*.a "$OUTPUT_DIR"/*.dylib # Remove individual arch objects and libs
echo "Intermediate files cleaned."

log_step "Compilation complete!"
echo "Your universal binaries are in the '$OUTPUT_DIR' directory."
echo "Executable: $OUTPUT_DIR/$EXECUTABLE_NAME"
echo "Static Library: $OUTPUT_DIR/$STATIC_LIB_NAME"
echo "Dynamic Library: $OUTPUT_DIR/$DYNAMIC_LIB_NAME"

