#!/bin/bash
# compile_kayte.sh - Kayte Lang Compilation Script
# Handles FPC path detection and proper compilation

set -e  # Exit on error

echo "╔═══════════════════════════════════════╗"
echo "║   Kayte Lang Compilation Script      ║"
echo "╚═══════════════════════════════════════╝"
echo ""

# Color codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Detect FPC compiler
echo "🔍 Detecting FreePascal compiler..."

if [ -f "/Users/pedro/fpcupdeluxe/fpc/bin/aarch64-darwin/fpc" ]; then
    FPC="/Users/pedro/fpcupdeluxe/fpc/bin/aarch64-darwin/fpc"
    echo -e "${GREEN}✓ Found fpcupdeluxe FPC${NC}"
elif [ -f "/usr/local/bin/fpc" ]; then
    FPC="/usr/local/bin/fpc"
    echo -e "${GREEN}✓ Found system FPC${NC}"
elif command -v fpc &> /dev/null; then
    FPC="fpc"
    echo -e "${GREEN}✓ Found FPC in PATH${NC}"
else
    echo -e "${RED}✗ FreePascal compiler not found!${NC}"
    echo ""
    echo "Please install FreePascal:"
    echo "  brew install fpc"
    echo "Or check your fpcupdeluxe installation."
    exit 1
fi

echo "  Using: $FPC"
echo ""

# Show version
echo "📦 FreePascal Version:"
$FPC -version | head -3
echo ""

# Check if we're in the right directory
if [ ! -f "projects/kayte.lpr" ]; then
    echo -e "${RED}✗ Error: kayte.lpr not found!${NC}"
    echo "Please run this script from the kayte-lang root directory"
    exit 1
fi

# Create build directories
echo "📁 Creating build directories..."
mkdir -p lib/aarch64-darwin
mkdir -p projects
echo -e "${GREEN}✓ Directories ready${NC}"
echo ""

# Compile
echo "🔨 Compiling Kayte Lang..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

$FPC \
    -Mobjfpc \
    -Scghi \
    -O3 \
    -Fu"src/lib" \
    -FE"projects" \
    -FU"lib/aarch64-darwin" \
    -o"projects/kayte" \
    projects/kayte.lpr

if [ $? -eq 0 ]; then
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo -e "${GREEN}✅ Compilation successful!${NC}"
    echo ""
    echo "Executable: projects/kayte"
    
    if [ -f "projects/kayte" ]; then
        SIZE=$(du -h projects/kayte | cut -f1)
        echo "Size: $SIZE"
    fi
    echo ""
    echo "Run with: ./projects/kayte"
else
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo -e "${RED}✗ Compilation failed${NC}"
    echo ""
    echo "Common issues:"
    echo "  1. Missing units - check 'uses' clauses"
    echo "  2. Syntax errors - review error messages above"
    echo "  3. Path issues - ensure src/lib contains all units"
    exit 1
fi
