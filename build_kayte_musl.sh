#!/bin/bash
# build_kayte_musl.sh
# Build script for Kayte language with musl libc support
# Supports Linux ARM64 (aarch64) and AMD64 (x86_64)

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_FILE="kayte.lpr"
PROJECT_NAME="kayte"

# Build directories
BUILD_DIR="$SCRIPT_DIR/build"
BIN_DIR="$SCRIPT_DIR/bin"
LIB_DIR="$SCRIPT_DIR/lib"

# musl toolchain paths
ARM64_MUSL_PREFIX="/opt/aarch64-linux-musl-cross"
AMD64_MUSL_PREFIX="/opt/x86_64-linux-musl-cross"

# Default target
TARGET="${1:-both}"

# Function to print colored messages
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check prerequisites
check_prerequisites() {
    print_info "Checking prerequisites..."
    
    if ! command_exists fpc; then
        print_error "FreePascal compiler (fpc) not found!"
        print_error "Install with: sudo apt-get install fpc"
        exit 1
    fi
    
    print_success "FreePascal found: $(fpc -iV)"
    
    # Check for musl toolchains
    if [ "$TARGET" = "arm64" ] || [ "$TARGET" = "both" ]; then
        if [ ! -d "$ARM64_MUSL_PREFIX" ]; then
            print_error "ARM64 musl toolchain not found at $ARM64_MUSL_PREFIX"
            print_error "Run setup_lazarus_musl.sh first"
            exit 1
        fi
        print_success "ARM64 musl toolchain found"
    fi
    
    if [ "$TARGET" = "amd64" ] || [ "$TARGET" = "both" ]; then
        if [ ! -d "$AMD64_MUSL_PREFIX" ]; then
            print_error "AMD64 musl toolchain not found at $AMD64_MUSL_PREFIX"
            print_error "Run setup_lazarus_musl.sh first"
            exit 1
        fi
        print_success "AMD64 musl toolchain found"
    fi
}

# Setup environment
setup_environment() {
    print_info "Setting up build environment..."
    
    # Add musl toolchains to PATH
    export PATH="$ARM64_MUSL_PREFIX/bin:$PATH"
    export PATH="$AMD64_MUSL_PREFIX/bin:$PATH"
    
    # Create build directories
    mkdir -p "$BUILD_DIR"
    mkdir -p "$BIN_DIR"
    mkdir -p "$LIB_DIR"
    
    print_success "Environment ready"
}

# Build for ARM64 with musl
build_arm64() {
    print_info "Building Kayte for Linux ARM64 with musl..."
    
    local OUTPUT_DIR="$BIN_DIR/aarch64-linux-musl"
    local LIB_OUTPUT="$LIB_DIR/aarch64-linux-musl"
    local OUTPUT_BIN="$OUTPUT_DIR/$PROJECT_NAME"
    
    mkdir -p "$OUTPUT_DIR"
    mkdir -p "$LIB_OUTPUT"
    
    # Compiler options for ARM64 musl
    local FPC_OPTS=(
        "-Tlinux"                                    # Target OS
        "-Paarch64"                                  # Target CPU
        "-XPaarch64-linux-musl-"                     # Binutils prefix
        "-Xd"                                        # Don't use standard library path
        "-Fl$ARM64_MUSL_PREFIX/aarch64-linux-musl/lib" # Library path
        "-FU$LIB_OUTPUT"                             # Unit output directory
        "-FE$OUTPUT_DIR"                             # Executable output directory
        "-o$OUTPUT_BIN"                              # Output filename
        "-CX"                                        # Create smartlinked units
        "-XX"                                        # Create smartlinked executable
        "-k-static"                                  # Static linking
        "-O3"                                        # Optimization level 3
        "-Xs"                                        # Strip symbols
        "-vh"                                        # Show notes and hints
    )
    
    print_info "Compiler options:"
    for opt in "${FPC_OPTS[@]}"; do
        echo "  $opt"
    done
    
    print_info "Compiling..."
    if fpc "${FPC_OPTS[@]}" "$PROJECT_FILE"; then
        print_success "ARM64 build successful!"
        
        # Verify the binary
        if [ -f "$OUTPUT_BIN" ]; then
            print_info "Binary info:"
            file "$OUTPUT_BIN"
            ls -lh "$OUTPUT_BIN"
            
            # Check if statically linked
            if ldd "$OUTPUT_BIN" 2>&1 | grep -q "not a dynamic executable"; then
                print_success "Binary is statically linked ✓"
            else
                print_warning "Binary may not be fully statically linked"
            fi
            
            # Strip the binary to reduce size
            aarch64-linux-musl-strip "$OUTPUT_BIN" 2>/dev/null || true
            print_info "Stripped binary size: $(du -h "$OUTPUT_BIN" | cut -f1)"
        else
            print_error "Binary not found at $OUTPUT_BIN"
            return 1
        fi
    else
        print_error "ARM64 build failed!"
        return 1
    fi
}

# Build for AMD64 with musl
build_amd64() {
    print_info "Building Kayte for Linux AMD64 with musl..."
    
    local OUTPUT_DIR="$BIN_DIR/x86_64-linux-musl"
    local LIB_OUTPUT="$LIB_DIR/x86_64-linux-musl"
    local OUTPUT_BIN="$OUTPUT_DIR/$PROJECT_NAME"
    
    mkdir -p "$OUTPUT_DIR"
    mkdir -p "$LIB_OUTPUT"
    
    # Compiler options for AMD64 musl
    local FPC_OPTS=(
        "-Tlinux"                                    # Target OS
        "-Px86_64"                                   # Target CPU
        "-XPx86_64-linux-musl-"                      # Binutils prefix
        "-Xd"                                        # Don't use standard library path
        "-Fl$AMD64_MUSL_PREFIX/x86_64-linux-musl/lib" # Library path
        "-FU$LIB_OUTPUT"                             # Unit output directory
        "-FE$OUTPUT_DIR"                             # Executable output directory
        "-o$OUTPUT_BIN"                              # Output filename
        "-CX"                                        # Create smartlinked units
        "-XX"                                        # Create smartlinked executable
        "-k-static"                                  # Static linking
        "-O3"                                        # Optimization level 3
        "-Xs"                                        # Strip symbols
        "-vh"                                        # Show notes and hints
    )
    
    print_info "Compiler options:"
    for opt in "${FPC_OPTS[@]}"; do
        echo "  $opt"
    done
    
    print_info "Compiling..."
    if fpc "${FPC_OPTS[@]}" "$PROJECT_FILE"; then
        print_success "AMD64 build successful!"
        
        # Verify the binary
        if [ -f "$OUTPUT_BIN" ]; then
            print_info "Binary info:"
            file "$OUTPUT_BIN"
            ls -lh "$OUTPUT_BIN"
            
            # Check if statically linked
            if ldd "$OUTPUT_BIN" 2>&1 | grep -q "not a dynamic executable"; then
                print_success "Binary is statically linked ✓"
            else
                print_warning "Binary may not be fully statically linked"
            fi
            
            # Strip the binary to reduce size
            x86_64-linux-musl-strip "$OUTPUT_BIN" 2>/dev/null || true
            print_info "Stripped binary size: $(du -h "$OUTPUT_BIN" | cut -f1)"
            
            # Test the binary (if on x86_64)
            if [ "$(uname -m)" = "x86_64" ]; then
                print_info "Testing binary..."
                if "$OUTPUT_BIN" --version; then
                    print_success "Binary test passed ✓"
                else
                    print_warning "Binary test failed"
                fi
            fi
        else
            print_error "Binary not found at $OUTPUT_BIN"
            return 1
        fi
    else
        print_error "AMD64 build failed!"
        return 1
    fi
}

# Clean build artifacts
clean_build() {
    print_info "Cleaning build artifacts..."
    rm -rf "$LIB_DIR"
    rm -rf "$BIN_DIR"
    rm -f *.o *.ppu *.compiled
    print_success "Clean complete"
}

# Show usage
show_usage() {
    echo "Usage: $0 [target]"
    echo ""
    echo "Targets:"
    echo "  arm64    - Build for Linux ARM64 (aarch64) with musl"
    echo "  amd64    - Build for Linux AMD64 (x86_64) with musl"
    echo "  both     - Build for both architectures (default)"
    echo "  clean    - Clean build artifacts"
    echo "  help     - Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0              # Build for both ARM64 and AMD64"
    echo "  $0 arm64        # Build for ARM64 only"
    echo "  $0 amd64        # Build for AMD64 only"
    echo "  $0 clean        # Clean build artifacts"
    echo ""
}

# Main script
main() {
    echo ""
    echo "=========================================="
    echo "  Kayte Language - musl Build Script"
    echo "=========================================="
    echo ""
    
    # Check if project file exists
    if [ ! -f "$PROJECT_FILE" ]; then
        print_error "Project file not found: $PROJECT_FILE"
        print_error "Please run this script from the project directory"
        exit 1
    fi
    
    case "$TARGET" in
        help|-h|--help)
            show_usage
            exit 0
            ;;
        clean)
            clean_build
            exit 0
            ;;
        arm64)
            check_prerequisites
            setup_environment
            build_arm64
            ;;
        amd64)
            check_prerequisites
            setup_environment
            build_amd64
            ;;
        both)
            check_prerequisites
            setup_environment
            build_arm64
            echo ""
            build_amd64
            ;;
        *)
            print_error "Unknown target: $TARGET"
            show_usage
            exit 1
            ;;
    esac
    
    echo ""
    echo "=========================================="
    print_success "Build completed!"
    echo "=========================================="
    echo ""
    
    # Show summary
    if [ -d "$BIN_DIR" ]; then
        print_info "Built binaries:"
        find "$BIN_DIR" -type f -name "$PROJECT_NAME" -exec echo "  {}" \;
    fi
    
    echo ""
}

# Run main
main
