#!/bin/bash
# build_kfm_lib.sh - Compile a .kfm form definition into a shared/static
# library (.dylib, .so, .dll, .a), mirroring build_mathlib_dylib.sh's
# pattern for mathlib.pas.
#
# Usage: ./build_kfm_lib.sh <input.kfm> [LibBaseName] [output-dir]
#
# Pipeline:
#   1. Compile projects/kfmlibgen.lpr once, if needed, into a `kfmlibgen`
#      tool (a small FPC program, not tied to any one .kfm file).
#   2. Run kfmlibgen on the input .kfm file to generate:
#        <LibBaseName>_impl.pas  - the form definition + exported API
#        <LibBaseName>_lib.lpr   - thin `library` wrapper around it
#   3. Compile <LibBaseName>_impl.pas alone and archive its .o into
#      lib<LibBaseName>.a (the static library).
#   4. Compile <LibBaseName>_lib.lpr for the host platform to produce the
#      native shared library (.dylib on macOS, .so on Linux); also
#      attempts Linux (.so) and Windows (.dll) cross-builds using the
#      cross-toolchains under ~/fpcupdeluxe/cross if present. Cross-build
#      failures are reported but don't abort the script - only the host
#      build is required to succeed.

set -e

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}╔═══════════════════════════════════════╗${NC}"
echo -e "${BLUE}║      .kfm Library Builder             ║${NC}"
echo -e "${BLUE}╚═══════════════════════════════════════╝${NC}"
echo ""

INPUT_KFM="$1"
LIB_BASE_NAME="${2:-$(basename "${INPUT_KFM%.*}")}"
OUT_DIR="${3:-build/kfm/$LIB_BASE_NAME}"

if [ -z "$INPUT_KFM" ]; then
    echo -e "${RED}✗ Usage: $0 <input.kfm> [LibBaseName] [output-dir]${NC}"
    exit 1
fi
if [ ! -f "$INPUT_KFM" ]; then
    echo -e "${RED}✗ Input .kfm file not found: $INPUT_KFM${NC}"
    exit 1
fi

# Detect FPC compiler. Prefer the fpcupdeluxe wrapper script (fpc -n
# @fpc.cfg), which pins fpcupdeluxe's own units and ignores any stray
# system-wide fpc.cfg - see compile_kayte.sh, which does the same.
echo "🔍 Detecting FreePascal compiler..."
if [ -f "/Users/pedro/fpcupdeluxe/fpc/bin/aarch64-darwin/fpc.sh" ]; then
    FPC="/Users/pedro/fpcupdeluxe/fpc/bin/aarch64-darwin/fpc.sh"
    HOST_ARCH="aarch64"
    echo -e "${GREEN}✓ Found fpcupdeluxe FPC (ARM64)${NC}"
elif [ -f "/usr/local/bin/fpc" ]; then
    FPC="/usr/local/bin/fpc"
    HOST_ARCH=$(uname -m)
    echo -e "${GREEN}✓ Found system FPC${NC}"
else
    echo -e "${RED}✗ FreePascal compiler not found!${NC}"
    exit 1
fi
echo "  Using: $FPC"
echo ""

HOST_OS=$(uname -s)
case "$HOST_OS" in
    Darwin) HOST_LIB_EXT="dylib" ;;
    Linux)  HOST_LIB_EXT="so" ;;
    *)      HOST_LIB_EXT="so" ;;
esac

mkdir -p "$OUT_DIR"

# Step 1: build kfmlibgen once
KFMLIBGEN="build/kfm/kfmlibgen"
if [ ! -x "$KFMLIBGEN" ]; then
    echo "🔨 Step 1/4: Building kfmlibgen (.kfm -> Pascal source generator)..."
    mkdir -p build/kfm
    $FPC -Mobjfpc -Sh -O2 -FUbuild/kfm -o"$KFMLIBGEN" projects/kfmlibgen.lpr
    echo -e "${GREEN}✓ kfmlibgen built${NC}"
else
    echo "🔨 Step 1/4: kfmlibgen already built, skipping"
fi
echo ""

# Step 2: generate <LibBaseName>_impl.pas and <LibBaseName>_lib.lpr
echo "🔨 Step 2/4: Generating library source from $INPUT_KFM..."
"$KFMLIBGEN" "$INPUT_KFM" "$OUT_DIR" "$LIB_BASE_NAME"
echo ""

IMPL_UNIT="${LIB_BASE_NAME}_impl"
LIB_PROJECT="${LIB_BASE_NAME}_lib"

# Step 3: static library (.a)
echo "🔨 Step 3/4: Building static library ($LIB_BASE_NAME.a)..."
$FPC -Mobjfpc -Sh -O2 -Fusource -FU"$OUT_DIR" "$OUT_DIR/$IMPL_UNIT.pas"
ar rcs "$OUT_DIR/lib$LIB_BASE_NAME.a" "$OUT_DIR/$IMPL_UNIT.o"
echo -e "${GREEN}✓ $OUT_DIR/lib$LIB_BASE_NAME.a${NC}"
echo ""

# Step 4: shared libraries (host + best-effort cross targets)
echo "🔨 Step 4/4: Building shared libraries..."

build_shared() {
    local label="$1" out_file="$2"; shift 2
    if $FPC -Mobjfpc -Sh -O2 -Fusource -FU"$OUT_DIR" -FE"$OUT_DIR" -o"$out_file" "$@" "$OUT_DIR/$LIB_PROJECT.lpr" >"$OUT_DIR/build_$label.log" 2>&1; then
        echo -e "${GREEN}✓ $label: $OUT_DIR/$out_file${NC}"
    else
        echo -e "${YELLOW}⚠ $label build failed or unavailable (see $OUT_DIR/build_$label.log)${NC}"
    fi
}

# Host target (required to be attempted; native .dylib/.so)
build_shared "host-$HOST_LIB_EXT" "$LIB_BASE_NAME.$HOST_LIB_EXT"

# Best-effort: Linux .so cross-build. Uses aarch64, matching the
# aarch64-linux RTL units actually installed under fpcupdeluxe (there's
# no x86_64-linux RTL on this machine, only an x86_64-linux binutils
# set, which is the wrong pairing) plus the Homebrew aarch64-linux-gnu
# binutils for the actual link step.
LINUX_CROSS="/opt/homebrew/bin"
if [ "$HOST_OS" != "Linux" ] && [ -x "$LINUX_CROSS/aarch64-linux-gnu-ld" ]; then
    build_shared "linux-so" "$LIB_BASE_NAME.so" -Tlinux -Paarch64 -FD"$LINUX_CROSS" -XPaarch64-linux-gnu-
fi

# Best-effort: Windows .dll cross-build (llvm-mingw)
MINGW_CROSS="/Users/pedro/fpcupdeluxe/cross/llvm-mingw/bin"
if [ -d "$MINGW_CROSS" ]; then
    build_shared "windows-dll" "$LIB_BASE_NAME.dll" -Twin64 -Px86_64 -FD"$MINGW_CROSS" -XP"x86_64-w64-mingw32-"
fi

echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🎉 Done. Artifacts (whichever succeeded) are in $OUT_DIR/"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
