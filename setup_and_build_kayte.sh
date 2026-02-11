#!/bin/bash
# setup_and_build_kayte.sh
# All-in-one script to setup musl toolchains and build Kayte
# This script handles everything: setup, environment, and building

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

echo ""
echo -e "${CYAN}============================================${NC}"
echo -e "${CYAN}  Kayte Language - musl Build Setup${NC}"
echo -e "${CYAN}============================================${NC}"
echo ""

# Configuration
MUSL_ARM64_URL="https://musl.cc/aarch64-linux-musl-cross.tgz"
MUSL_AMD64_URL="https://musl.cc/x86_64-linux-musl-cross.tgz"
INSTALL_DIR="/opt"
ARM64_DIR="$INSTALL_DIR/aarch64-linux-musl-cross"
AMD64_DIR="$INSTALL_DIR/x86_64-linux-musl-cross"

PROJECT_FILE="kayte.lpr"
BUILD_TARGET="${1:-both}"

# Check if running as root for installation
if [ "$EUID" -ne 0 ] && [ ! -w "$INSTALL_DIR" ]; then
    echo -e "${RED}[ERROR]${NC} This script needs sudo privileges to install toolchains to $INSTALL_DIR"
    echo "Please run: sudo $0 $BUILD_TARGET"
    exit 1
fi

# Step 1: Check prerequisites
echo -e "${BLUE}[STEP 1/5]${NC} Checking prerequisites..."

if ! command -v fpc >/dev/null 2>&1; then
    echo -e "${RED}[ERROR]${NC} FreePascal compiler not found!"
    echo "Installing FreePascal..."
    apt-get update && apt-get install -y fpc || {
        echo -e "${RED}[ERROR]${NC} Failed to install FreePascal"
        exit 1
    }
fi

echo -e "${GREEN}[OK]${NC} FreePascal $(fpc -iV) found"

if ! command -v wget >/dev/null 2>&1; then
    echo "Installing wget..."
    apt-get install -y wget
fi

# Step 2: Install musl toolchains
echo ""
echo -e "${BLUE}[STEP 2/5]${NC} Installing musl toolchains..."

# ARM64 toolchain
if [ ! -d "$ARM64_DIR" ] && ( [ "$BUILD_TARGET" = "arm64" ] || [ "$BUILD_TARGET" = "both" ] ); then
    echo -e "${YELLOW}[INSTALL]${NC} Downloading ARM64 musl toolchain..."
    cd /tmp
    wget -q --show-progress -O aarch64-musl.tgz "$MUSL_ARM64_URL"
    echo -e "${YELLOW}[INSTALL]${NC} Extracting ARM64 toolchain..."
    tar -xzf aarch64-musl.tgz
    mv aarch64-linux-musl-cross "$ARM64_DIR"
    rm aarch64-musl.tgz
    echo -e "${GREEN}[OK]${NC} ARM64 musl toolchain installed"
else
    echo -e "${GREEN}[OK]${NC} ARM64 musl toolchain already installed"
fi

# AMD64 toolchain
if [ ! -d "$AMD64_DIR" ] && ( [ "$BUILD_TARGET" = "amd64" ] || [ "$BUILD_TARGET" = "both" ] ); then
    echo -e "${YELLOW}[INSTALL]${NC} Downloading AMD64 musl toolchain..."
    cd /tmp
    wget -q --show-progress -O x86_64-musl.tgz "$MUSL_AMD64_URL"
    echo -e "${YELLOW}[INSTALL]${NC} Extracting AMD64 toolchain..."
    tar -xzf x86_64-musl.tgz
    mv x86_64-linux-musl-cross "$AMD64_DIR"
    rm x86_64-musl.tgz
    echo -e "${GREEN}[OK]${NC} AMD64 musl toolchain installed"
else
    echo -e "${GREEN}[OK]${NC} AMD64 musl toolchain already installed"
fi

# Step 3: Setup environment
echo ""
echo -e "${BLUE}[STEP 3/5]${NC} Setting up environment..."

export PATH="$ARM64_DIR/bin:$PATH"
export PATH="$AMD64_DIR/bin:$PATH"

echo -e "${GREEN}[OK]${NC} Environment configured"

# Step 4: Verify project
echo ""
echo -e "${BLUE}[STEP 4/5]${NC} Verifying project files..."

if [ ! -f "$PROJECT_FILE" ]; then
    echo -e "${RED}[ERROR]${NC} Project file not found: $PROJECT_FILE"
    echo "Please run this script from the Kayte project directory"
    exit 1
fi

echo -e "${GREEN}[OK]${NC} Project file found: $PROJECT_FILE"

# Create directories
mkdir -p bin/aarch64-linux-musl
mkdir -p bin/x86_64-linux-musl
mkdir -p lib/aarch64-linux-musl
mkdir -p lib/x86_64-linux-musl

# Step 5: Build
echo ""
echo -e "${BLUE}[STEP 5/5]${NC} Building Kayte..."
echo ""

build_arm64() {
    echo -e "${CYAN}[BUILD ARM64]${NC} Compiling for Linux ARM64 with musl..."
    
    fpc \
        -Tlinux \
        -Paarch64 \
        -XPaarch64-linux-musl- \
        -Xd \
        -Fl"$ARM64_DIR/aarch64-linux-musl/lib" \
        -FUlib/aarch64-linux-musl \
        -FEbin/aarch64-linux-musl \
        -obin/aarch64-linux-musl/kayte \
        -k-static \
        -O3 \
        -CX \
        -XX \
        -Xs \
        -vh \
        "$PROJECT_FILE"
    
    if [ -f "bin/aarch64-linux-musl/kayte" ]; then
        echo -e "${GREEN}[SUCCESS]${NC} ARM64 build complete"
        
        # Strip binary
        aarch64-linux-musl-strip bin/aarch64-linux-musl/kayte 2>/dev/null || true
        
        # Show info
        echo -e "${BLUE}[INFO]${NC} Binary: $(file bin/aarch64-linux-musl/kayte)"
        echo -e "${BLUE}[INFO]${NC} Size: $(du -h bin/aarch64-linux-musl/kayte | cut -f1)"
        
        # Check static linking
        if ldd bin/aarch64-linux-musl/kayte 2>&1 | grep -q "not a dynamic"; then
            echo -e "${GREEN}[OK]${NC} Statically linked ✓"
        else
            echo -e "${YELLOW}[WARN]${NC} May not be fully static"
        fi
    else
        echo -e "${RED}[ERROR]${NC} ARM64 build failed"
        return 1
    fi
}

build_amd64() {
    echo -e "${CYAN}[BUILD AMD64]${NC} Compiling for Linux AMD64 with musl..."
    
    fpc \
        -Tlinux \
        -Px86_64 \
        -XPx86_64-linux-musl- \
        -Xd \
        -Fl"$AMD64_DIR/x86_64-linux-musl/lib" \
        -FUlib/x86_64-linux-musl \
        -FEbin/x86_64-linux-musl \
        -obin/x86_64-linux-musl/kayte \
        -k-static \
        -O3 \
        -CX \
        -XX \
        -Xs \
        -vh \
        "$PROJECT_FILE"
    
    if [ -f "bin/x86_64-linux-musl/kayte" ]; then
        echo -e "${GREEN}[SUCCESS]${NC} AMD64 build complete"
        
        # Strip binary
        x86_64-linux-musl-strip bin/x86_64-linux-musl/kayte 2>/dev/null || true
        
        # Show info
        echo -e "${BLUE}[INFO]${NC} Binary: $(file bin/x86_64-linux-musl/kayte)"
        echo -e "${BLUE}[INFO]${NC} Size: $(du -h bin/x86_64-linux-musl/kayte | cut -f1)"
        
        # Check static linking
        if ldd bin/x86_64-linux-musl/kayte 2>&1 | grep -q "not a dynamic"; then
            echo -e "${GREEN}[OK]${NC} Statically linked ✓"
        else
            echo -e "${YELLOW}[WARN]${NC} May not be fully static"
        fi
        
        # Test if on x86_64
        if [ "$(uname -m)" = "x86_64" ]; then
            echo -e "${BLUE}[TEST]${NC} Testing binary..."
            if bin/x86_64-linux-musl/kayte --version 2>/dev/null; then
                echo -e "${GREEN}[OK]${NC} Binary test passed ✓"
            fi
        fi
    else
        echo -e "${RED}[ERROR]${NC} AMD64 build failed"
        return 1
    fi
}

# Execute builds based on target
case "$BUILD_TARGET" in
    arm64)
        build_arm64
        ;;
    amd64)
        build_amd64
        ;;
    both)
        build_arm64
        echo ""
        build_amd64
        ;;
    *)
        echo -e "${RED}[ERROR]${NC} Unknown target: $BUILD_TARGET"
        echo "Valid targets: arm64, amd64, both"
        exit 1
        ;;
esac

# Final summary
echo ""
echo -e "${CYAN}============================================${NC}"
echo -e "${GREEN}  Build Complete!${NC}"
echo -e "${CYAN}============================================${NC}"
echo ""

if [ -d "bin" ]; then
    echo -e "${BLUE}[INFO]${NC} Built binaries:"
    find bin -type f -name "kayte" -exec ls -lh {} \;
fi

echo ""
echo -e "${GREEN}Next steps:${NC}"
echo "  1. Test your binaries:"
echo "     ./bin/x86_64-linux-musl/kayte --version"
if command -v qemu-aarch64-static >/dev/null 2>&1; then
    echo "     qemu-aarch64-static ./bin/aarch64-linux-musl/kayte --version"
else
    echo "     (Install qemu-user-static to test ARM64 binary)"
fi
echo ""
echo "  2. Install system-wide (optional):"
echo "     sudo cp bin/x86_64-linux-musl/kayte /usr/local/bin/kayte"
echo ""
echo "  3. Deploy to other systems:"
echo "     scp bin/x86_64-linux-musl/kayte user@server:/usr/local/bin/"
echo ""

echo -e "${CYAN}============================================${NC}"
echo ""
