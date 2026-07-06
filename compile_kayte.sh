#!/bin/bash
# compile_kayte.sh - Kayte Lang Compilation Script
# Builds the projects/kayte.lpr project (macOS, native arch - Apple Silicon
# arm64 on this machine) using the FreePascal Compiler directly (no lazbuild
# dependency).
#
# By default this builds the core language only: lexer, parser, compiler,
# assembler, VM/bytecode and the native backends. It has no third-party
# unit dependencies beyond the standard FPC RTL, so it builds the same way
# on any machine with a working FPC install.
#
# Pass --http to additionally build the optional HTTP server feature
# (`kayte --http`). That pulls in fcl-web/fcl-net, which is a much heavier
# and more fragile dependency (see EXTRA_FU below) - it's opt-in on purpose.

set -e  # Exit on error

WITH_HTTP=0
for arg in "$@"; do
  case "$arg" in
    --http) WITH_HTTP=1 ;;
  esac
done

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

FPC=""
if [ -f "/Users/pedro/fpcupdeluxe/fpc/bin/aarch64-darwin/fpc.sh" ]; then
    # Use the fpcupdeluxe wrapper: it points -n at this install's own
    # fpc.cfg, avoiding mismatches with any other fpc.cfg on the system
    # (e.g. /etc/fpc.cfg from a different FPC version/install).
    FPC="/Users/pedro/fpcupdeluxe/fpc/bin/aarch64-darwin/fpc.sh"
    FPCSRC="/Users/pedro/fpcupdeluxe/fpcsrc"
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
mkdir -p projects/lib/aarch64-darwin
echo -e "${GREEN}✓ Directories ready${NC}"
echo ""

DEFINES=()
EXTRA_FU=()
if [ "$WITH_HTTP" = "1" ]; then
    echo -e "${YELLOW}Building with optional HTTP server support (-dKAYTE_HTTP)${NC}"
    DEFINES+=(-dKAYTE_HTTP)

    # fcl-web/fcl-net precompiled units are, on some fpcupdeluxe installs,
    # stale or partial (e.g. missing cNetDB, or fphttpclient's cached .ppu
    # out of sync with ssockets.ppu), which forces the compiler to
    # recompile them from source. When that happens the .pp sources must
    # be reachable, so we add them if present. This is a no-op (harmless)
    # when the precompiled units are already consistent, and only needed
    # at all when --http is requested.
    if [ -n "$FPCSRC" ]; then
        [ -d "$FPCSRC/packages/fcl-net/src" ] && EXTRA_FU+=(-Fu"$FPCSRC/packages/fcl-net/src")
        [ -d "$FPCSRC/packages/fcl-web/src/base" ] && EXTRA_FU+=(-Fu"$FPCSRC/packages/fcl-web/src/base")
    fi
    echo ""
fi

# Compile
echo "🔨 Compiling Kayte Lang..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

(
  cd projects
  "$FPC" \
      -Mobjfpc \
      -Scghi \
      -O2 \
      "${DEFINES[@]}" \
      -Fu"../source" \
      -Fu"../jvm" \
      "${EXTRA_FU[@]}" \
      -FE"." \
      -FU"lib/aarch64-darwin" \
      -o"kayte" \
      kayte.lpr
)

if [ $? -eq 0 ]; then
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo -e "${GREEN}✅ Compilation successful!${NC}"
    echo ""
    echo "Executable: projects/kayte"

    if [ -f "projects/kayte" ]; then
        SIZE=$(du -h projects/kayte | cut -f1)
        echo "Size: $SIZE"
        file projects/kayte
    fi
    echo ""
    echo "Run with: ./projects/kayte"
    if [ "$WITH_HTTP" != "1" ]; then
        echo "(built without HTTP server support; re-run with --http to enable it)"
    fi
else
    echo ""
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo -e "${RED}✗ Compilation failed${NC}"
    echo ""
    echo "Common issues:"
    echo "  1. Missing units - check 'uses' clauses"
    echo "  2. Syntax errors - review error messages above"
    echo "  3. Path issues - ensure source/ contains all units"
    exit 1
fi
