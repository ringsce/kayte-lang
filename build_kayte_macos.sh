#!/bin/bash
#
# build_kayte_macos.sh
#
# This script compiles the Kayte language project for macOS,
# automatically detecting the architecture (x86_64 or arm64)
# and producing universal (fat) binaries.
# It now includes DWARF debug information.
#
# Dependencies:
# - Xcode Command Line Tools (clang, ar, lipo, lldb)
# - kayte_runtime.c
# - kayte_program_x86_64.ll (or a universal .ll if your compiler generates it)

# Exit immediately if a command exits with a non-zero status.
set -e

# --- Configuration ---
RUNTIME_C_FILE="kayte_runtime.c"
# For this script, we'll use the x86_64 LLVM IR as the base for both targets.
# In a real compiler, you'd generate the correct target triple in the .ll file.
LL_SOURCE_FILE="kayte_program_x86_64.ll" # Use the LLVM IR with debug info

OUTPUT_DIR="build_macos_debug"
EXECUTABLE_NAME="kayte_app_debug"
STATIC_LIB_NAME="libkayte_universal_debug.a"
DYNAMIC_LIB_NAME="libkayte_universal_debug.dylib"

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
check_command "lldb"

if [[ ! -f "$RUNTIME_C_FILE" ]]; then
    echo "Error: $RUNTIME_C_FILE not found in the current directory."
    exit 1
fi
if [[ ! -f "$LL_SOURCE_FILE" ]]; then
    echo "Error: $LL_SOURCE_FILE not found in the current directory."
    echo "Please ensure 'kayte_program_x86_64.ll' (or your debug-enabled .ll) is present."
    exit 1
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"
echo "Output directory: $OUTPUT_DIR"

log_step "Compiling Kayte Runtime ($RUNTIME_C_FILE) for x86_64 and arm64 with debug info..."
# -g: Emit debug info
clang -g -c "$RUNTIME_C_FILE" -target x86_64-apple-macosx14.0.0 -o "$OUTPUT_DIR/kayte_runtime_x86_64.o"
clang -g -c "$RUNTIME_C_FILE" -target arm64-apple-macosx14.0.0 -o "$OUTPUT_DIR/kayte_runtime_arm64.o"
echo "Runtime compiled for both architectures with debug info."

log_step "Compiling Kayte LLVM IR ($LL_SOURCE_FILE) for x86_64 and arm64 with debug info..."
# -g: Emit debug info
# -target: Explicitly set target triple for llc/clang when compiling .ll
clang -g -c "$LL_SOURCE_FILE" -target x86_64-apple-macosx14.0.0 -o "$OUTPUT_DIR/kayte_program_x86_64.o"
clang -g -c "$LL_SOURCE_FILE" -target arm64-apple-macosx14.0.0 -o "$OUTPUT_DIR/kayte_program_arm64.o"
echo "Kayte program compiled for both architectures with debug info."

log_step "Creating Universal Executable ($EXECUTABLE_NAME) with debug info..."
# -g: Include debug info in the final executable
clang -g "$OUTPUT_DIR/kayte_program_x86_64.o" "$OUTPUT_DIR/kayte_runtime_x86_64.o" \
      "$OUTPUT_DIR/kayte_program_arm64.o" "$OUTPUT_DIR/kayte_runtime_arm64.o" \
      -o "$OUTPUT_DIR/$EXECUTABLE_NAME" \
      -lSystem -lc # Link with standard system libraries
echo "Universal executable created: $OUTPUT_DIR/$EXECUTABLE_NAME"
echo "You can run it with: $OUTPUT_DIR/$EXECUTABLE_NAME"

log_step "Creating Universal Static Library ($STATIC_LIB_NAME) with debug info..."
# Create individual static libs first
ar rcs "$OUTPUT_DIR/libkayte_x86_64_debug.a" "$OUTPUT_DIR/kayte_program_x86_64.o" "$OUTPUT_DIR/kayte_runtime_x86_64.o"
ar rcs "$OUTPUT_DIR/libkayte_arm64_debug.a" "$OUTPUT_DIR/kayte_program_arm64.o" "$OUTPUT_DIR/kayte_runtime_arm64.o"
# Lipo them into a universal static library
lipo -create "$OUTPUT_DIR/libkayte_x86_64_debug.a" "$OUTPUT_DIR/libkayte_arm64_debug.a" -output "$OUTPUT_DIR/$STATIC_LIB_NAME"
echo "Universal static library created: $OUTPUT_DIR/$STATIC_LIB_NAME"

log_step "Creating Universal Dynamic Library ($DYNAMIC_LIB_NAME) with debug info..."
# Create individual dynamic libs first
clang -g -shared -o "$OUTPUT_DIR/libkayte_x86_64_debug.dylib" "$OUTPUT_DIR/kayte_program_x86_64.o" "$OUTPUT_DIR/kayte_runtime_x86_64.o" -lSystem -lc
clang -g -shared -o "$OUTPUT_DIR/libkayte_arm64_debug.dylib" "$OUTPUT_DIR/kayte_program_arm64.o" "$OUTPUT_DIR/kayte_runtime_arm64.o" -lSystem -lc
# Lipo them into a universal dynamic library
lipo -create "$OUTPUT_DIR/libkayte_x86_64_debug.dylib" "$OUTPUT_DIR/libkayte_arm64_debug.dylib" -output "$OUTPUT_DIR/$DYNAMIC_LIB_NAME"
echo "Universal dynamic library created: $OUTPUT_DIR/$DYNAMIC_LIB_NAME"

log_step "Cleaning up intermediate object files..."
# Remove individual arch objects and libs (keep the universal ones)
rm "$OUTPUT_DIR"/*.o "$OUTPUT_DIR"/*_debug.a "$OUTPUT_DIR"/*_debug.dylib
echo "Intermediate files cleaned."

log_step "Compilation complete!"
echo "Your universal binaries with DWARF debug info are in the '$OUTPUT_DIR' directory."
echo "Executable: $OUTPUT_DIR/$EXECUTABLE_NAME"
echo "Static Library: $OUTPUT_DIR/$STATIC_LIB_NAME"
echo "Dynamic Library: $OUTPUT_DIR/$DYNAMIC_LIB_NAME"

log_step "How to debug with LLDB:"
echo "1. Navigate to the output directory: cd $OUTPUT_DIR"
echo "2. Start LLDB: lldb $EXECUTABLE_NAME"
echo "3. In LLDB, set a breakpoint: b kayte_main"
echo "   (or 'b kayte_program.kayte:9' for line 9 of your Kayte source)"
echo "4. Run the program: r"
echo "5. Use 'n' (next), 's' (step), 'v' (view variables), 'bt' (backtrace) to debug."
echo "6. To quit: q"

