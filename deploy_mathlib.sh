#!/bin/bash
# deploy_mathlib.sh
# Deployment script for MathLib to Kayte Lang project
# Usage: ./deploy_mathlib.sh

set -e

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  MathLib Deployment to Kayte Lang${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Detect Kayte Lang project directory
KAYTE_PROJECT=""

if [ -d "/Users/pedro/Projects/kayte-lang" ]; then
    KAYTE_PROJECT="/Users/pedro/Projects/kayte-lang"
elif [ -d "$HOME/Projects/kayte-lang" ]; then
    KAYTE_PROJECT="$HOME/Projects/kayte-lang"
elif [ -d "./kayte-lang" ]; then
    KAYTE_PROJECT="./kayte-lang"
else
    echo -e "${RED}Error: Could not find Kayte Lang project directory${NC}"
    echo "Please specify the path manually:"
    read -p "Enter Kayte Lang project path: " KAYTE_PROJECT
fi

if [ ! -d "$KAYTE_PROJECT" ]; then
    echo -e "${RED}Error: Directory does not exist: $KAYTE_PROJECT${NC}"
    exit 1
fi

echo -e "${GREEN}Found Kayte Lang project: $KAYTE_PROJECT${NC}"
echo ""

# Create directories if they don't exist
echo "Creating directory structure..."
mkdir -p "$KAYTE_PROJECT/src/lib"
mkdir -p "$KAYTE_PROJECT/src/runtime"
mkdir -p "$KAYTE_PROJECT/tests"
mkdir -p "$KAYTE_PROJECT/examples"
echo -e "${GREEN}✓ Directories created${NC}"
echo ""

# Copy MathLib files
echo "Copying MathLib files..."

if [ -f "MathLib.pas" ]; then
    cp MathLib.pas "$KAYTE_PROJECT/src/lib/"
    echo -e "${GREEN}✓ Copied MathLib.pas to src/lib/${NC}"
else
    echo -e "${RED}✗ MathLib.pas not found${NC}"
    exit 1
fi

# Copy optional files if they exist
if [ -f "KayteMathBindings.pas" ]; then
    cp KayteMathBindings.pas "$KAYTE_PROJECT/src/runtime/"
    echo -e "${GREEN}✓ Copied KayteMathBindings.pas to src/runtime/${NC}"
fi

if [ -f "MathLibTest.pas" ]; then
    cp MathLibTest.pas "$KAYTE_PROJECT/tests/"
    echo -e "${GREEN}✓ Copied MathLibTest.pas to tests/${NC}"
fi

if [ -f "math_example.kayte" ]; then
    cp math_example.kayte "$KAYTE_PROJECT/examples/"
    echo -e "${GREEN}✓ Copied math_example.kayte to examples/${NC}"
fi

if [ -f "README.md" ]; then
    cp README.md "$KAYTE_PROJECT/docs/MathLib_README.md" 2>/dev/null || cp README.md "$KAYTE_PROJECT/"
    echo -e "${GREEN}✓ Copied README.md${NC}"
fi

if [ -f "INTEGRATION.md" ]; then
    cp INTEGRATION.md "$KAYTE_PROJECT/docs/MathLib_INTEGRATION.md" 2>/dev/null || cp INTEGRATION.md "$KAYTE_PROJECT/"
    echo -e "${GREEN}✓ Copied INTEGRATION.md${NC}"
fi

echo ""
echo -e "${BLUE}========================================${NC}"
echo -e "${GREEN}Deployment Complete!${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""
echo "Files have been copied to:"
echo "  • $KAYTE_PROJECT/src/lib/MathLib.pas"
echo "  • $KAYTE_PROJECT/src/runtime/KayteMathBindings.pas"
echo "  • $KAYTE_PROJECT/tests/MathLibTest.pas"
echo "  • $KAYTE_PROJECT/examples/math_example.kayte"
echo ""
echo "Next steps:"
echo "  1. Add MathLib to your project's uses clause"
echo "  2. Rebuild your project with: fpc -Fu\"src/lib\" kayte.lpr"
echo "  3. Test the library with: cd tests && fpc MathLibTest.pas && ./MathLibTest"
echo ""
echo "For detailed integration instructions, see INTEGRATION.md"
echo ""
