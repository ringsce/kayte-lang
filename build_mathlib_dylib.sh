#!/bin/bash
# build_mathlib_dylib.sh - Build MathLib as Dynamic Library
# For Kayte Lang Project

set -e  # Exit on error

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}╔═══════════════════════════════════════╗${NC}"
echo -e "${BLUE}║   MathLib Dynamic Library Builder    ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════╝${NC}"
echo ""

# Detect FPC compiler
echo "🔍 Detecting FreePascal compiler..."

if [ -f "/Users/pedro/fpcupdeluxe/fpc/bin/aarch64-darwin/fpc" ]; then
    FPC="/Users/pedro/fpcupdeluxe/fpc/bin/aarch64-darwin/fpc"
    ARCH="aarch64"
    echo -e "${GREEN}✓ Found fpcupdeluxe FPC (ARM64)${NC}"
elif [ -f "/usr/local/bin/fpc" ]; then
    FPC="/usr/local/bin/fpc"
    ARCH=$(uname -m)
    echo -e "${GREEN}✓ Found system FPC${NC}"
else
    echo -e "${RED}✗ FreePascal compiler not found!${NC}"
    exit 1
fi

echo "  Using: $FPC"
echo "  Architecture: $ARCH"
echo ""

# Check for required files
echo "📋 Checking required files..."

if [ ! -f "source/mathlib.pas" ]; then
    echo -e "${RED}✗ source/mathlib.pas not found!${NC}"
    echo "Please ensure mathlib.pas is in the source/ directory"
    exit 1
fi

if [ ! -f "source/mathlibdylib.pas" ]; then
    echo -e "${RED}✗ source/mathlibdylib.pas not found!${NC}"
    echo "Please ensure mathlibdylib.pas is in the source/ directory"
    exit 1
fi

echo -e "${GREEN}✓ All required files present${NC}"
echo ""

# Create directories
echo "📁 Creating build directories..."
mkdir -p lib/$ARCH-darwin
mkdir -p source/lib/$ARCH-darwin
echo -e "${GREEN}✓ Directories created${NC}"
echo ""

# Compile mathlib.pas first
echo "🔨 Step 1/2: Compiling mathlib.pas..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

$FPC -Mobjfpc -Sh -O3 \
    -FUsource/lib/$ARCH-darwin \
    source/mathlib.pas

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ mathlib.pas compiled successfully${NC}"
else
    echo -e "${RED}✗ Failed to compile mathlib.pas${NC}"
    exit 1
fi
echo ""

# Compile dynamic library
echo "🔨 Step 2/2: Building dynamic library..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

$FPC -Mobjfpc -Sh -O3 \
    -Fusource/lib/$ARCH-darwin \
    -FUlib/$ARCH-darwin \
    -olibmathlib.dylib \
    source/mathlibdylib.pas

if [ $? -eq 0 ]; then
    echo ""
    echo -e "${GREEN}✅ Dynamic library built successfully!${NC}"
    echo ""
    
    # Show library info
    if [ -f "libmathlib.dylib" ]; then
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        echo "📦 Library Information:"
        echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
        
        # Get file size
        SIZE=$(du -h libmathlib.dylib | cut -f1)
        echo "  Location: $(pwd)/libmathlib.dylib"
        echo "  Size: $SIZE"
        
        # Check exported symbols
        echo ""
        echo "  Exported functions: $(nm -g libmathlib.dylib | grep " T " | wc -l | xargs)"
        echo ""
        
        # Show some sample exports
        echo "  Sample exports:"
        nm -g libmathlib.dylib | grep " T " | head -5 | while read line; do
            func=$(echo $line | awk '{print $3}')
            echo "    - $func"
        done
        echo "    ... (and more)"
        echo ""
        
        # Move to lib directory
        mkdir -p lib
        mv libmathlib.dylib lib/
        echo -e "${GREEN}✓ Library moved to lib/libmathlib.dylib${NC}"
    fi
    
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "🎉 Build Complete!"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "Next steps:"
    echo "  1. Test the library:"
    echo "     ./test_mathlib_dylib"
    echo ""
    echo "  2. Install system-wide (optional):"
    echo "     sudo cp lib/libmathlib.dylib /usr/local/lib/"
    echo ""
    echo "  3. Use in your Kayte project:"
    echo "     Add to kayte.lpr uses clause:"
    echo "     uses MathLib;"
    echo ""
else
    echo ""
    echo -e "${RED}✗ Failed to build dynamic library${NC}"
    echo ""
    echo "Common issues:"
    echo "  - Missing mathlib.ppu: Compile mathlib.pas first"
    echo "  - Wrong paths: Check source/ directory structure"
    exit 1
fi
