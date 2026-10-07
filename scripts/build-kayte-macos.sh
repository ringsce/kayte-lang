#!/usr/bin/env bash
#
# build-kayte-macos.sh
#
# Native macOS build: unlike scripts/build-kayte-debian-container.sh and
# scripts/build-kayte-alpine-container.sh (which spin up a Linux container
# via Apple's `container` CLI because those targets are Linux), macOS
# binaries are built directly on the host - no container/VM needed, since
# we're already on macOS. This script locates fpc + lazbuild, compiles the
# native Mach-O backend object (source/kayte_arm64_emit.c), then builds
# Kayte (source/kayte.lpi) with lazbuild, which statically links it in
# (see source/kaytearm64.pas).
#
# Usage:
#   scripts/build-kayte-macos.sh [command]
#
# Commands:
#   build   (default) compile the native Mach-O backend object and
#           source/kayte.lpi, copying the resulting binary out.
#   vb6     same, but for source/vb6interpreter.lpi (an LCL app - on
#           macOS this builds against the Cocoa widgetset).
#   webgen  same, but for source/webgen.lpi (an LCL app - on
#           macOS this builds against the Cocoa widgetset).
#   clean   remove build/macos and the macOS-specific intermediate files
#           left behind in the repo (source/kayte_arm64_emit.o, lib/*.dylib,
#           bin/kayte, bin/vb6interpreter, bin/webgen, build/units). Does
#           NOT touch build/debian or build/alpine.
#   help    show this usage text.
#
# Requirements (not installed automatically unless brew is available):
#   fpc       FreePascal compiler - already on PATH, or installed via
#             fpcupdeluxe / `brew install fpc`.
#   lazbuild  headless Lazarus project builder - looked up on PATH and in
#             common install locations (e.g. /Applications/lazarus), or
#             installed via `brew install --cask lazarus`.

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_info()    { echo -e "${BLUE}[INFO]${NC} $1"; }
print_success() { echo -e "${GREEN}[OK]${NC} $1"; }
print_warn()    { echo -e "${YELLOW}[WARN]${NC} $1"; }
print_error()   { echo -e "${RED}[ERROR]${NC} $1"; }

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

OUT_DIR="$REPO_ROOT/build/macos"

COMMAND="${1:-build}"

LAZBUILD=""

require_host() {
    if [ "$(uname -s)" != "Darwin" ]; then
        print_error "This script builds native macOS binaries and only runs on macOS."
        exit 1
    fi
    if [ "$(uname -m)" != "arm64" ]; then
        print_warn "The native Mach-O backend (source/kayte_arm64_emit.c) targets Apple Silicon; $(uname -m) may not build correctly."
    fi
}

ensure_fpc() {
    if command -v fpc >/dev/null 2>&1; then
        print_success "fpc found: $(fpc -iV 2>/dev/null)"
        return
    fi

    print_info "fpc not found on PATH, attempting to install..."
    if command -v brew >/dev/null 2>&1; then
        brew install fpc
    else
        print_error "Homebrew not found. Install FreePascal manually (fpcupdeluxe or https://www.freepascal.org/) and re-run."
        exit 1
    fi

    if ! command -v fpc >/dev/null 2>&1; then
        print_error "fpc still not on PATH after install."
        exit 1
    fi
    print_success "fpc installed."
}

find_lazbuild() {
    if command -v lazbuild >/dev/null 2>&1; then
        command -v lazbuild
        return 0
    fi
    for candidate in /Applications/lazarus/lazbuild /Applications/Lazarus/lazbuild "$HOME/fpcupdeluxe/lazarus/lazbuild"; do
        if [ -x "$candidate" ]; then
            echo "$candidate"
            return 0
        fi
    done
    return 1
}

ensure_lazbuild() {
    if LAZBUILD="$(find_lazbuild)"; then
        print_success "lazbuild found: $LAZBUILD ($("$LAZBUILD" --version 2>/dev/null))"
        return
    fi

    print_info "lazbuild not found, attempting to install Lazarus..."
    if command -v brew >/dev/null 2>&1; then
        brew install --cask lazarus
    else
        print_error "Homebrew not found. Install Lazarus manually (https://www.lazarus-ide.org/) and re-run."
        exit 1
    fi

    if ! LAZBUILD="$(find_lazbuild)"; then
        print_error "lazbuild still not found after installing Lazarus."
        exit 1
    fi
    print_success "lazbuild installed: $LAZBUILD"
}

compile_kayte() {
    mkdir -p "$OUT_DIR"

    # source/kaytearm64.pas statically links 'kayte_arm64_emit.o' (i.e.
    # a plain object file at the repo root, not a .dylib) on Darwin/AArch64
    # - see the build note at the top of that unit.
    print_info "Compiling kayte_arm64_emit.o (native Mach-O backend)..."
    clang -c -O2 -std=c11 "$REPO_ROOT/source/kayte_arm64_emit.c" -o "$REPO_ROOT/source/kayte_arm64_emit.o"

    print_info "Building source/kayte.lpi with lazbuild..."
    (cd "$REPO_ROOT/source" && "$LAZBUILD" kayte.lpi)

    if [ -f "$REPO_ROOT/bin/kayte" ]; then
        cp -f "$REPO_ROOT/bin/kayte" "$OUT_DIR/kayte"
        print_success "Built: $OUT_DIR/kayte"
    else
        print_error "Build finished but output binary was not found."
        exit 1
    fi
}

compile_vb6interpreter() {
    mkdir -p "$OUT_DIR"
    print_info "Building source/vb6interpreter.lpi with lazbuild..."
    # Xcode's newer linker chokes on the prebuilt Cocoa LCL units shipped
    # with the Lazarus cask ("malformed method list atom ... fixups found
    # beyond the number of method entries" in cocoawsextctrls.o and
    # similar). Force the classic linker to work around it.
    (cd "$REPO_ROOT/source" && "$LAZBUILD" --opt="-k-ld_classic" vb6interpreter.lpi)

    if [ -f "$REPO_ROOT/bin/vb6interpreter" ]; then
        cp -f "$REPO_ROOT/bin/vb6interpreter" "$OUT_DIR/vb6interpreter-macos-$(uname -m)"
        print_success "Built: $OUT_DIR/vb6interpreter-macos-$(uname -m)"
    else
        print_error "Build finished but output binary was not found."
        exit 1
    fi
}

compile_webgen() {
    mkdir -p "$OUT_DIR"
    print_info "Building source/webgen.lpi with lazbuild..."
    # Xcode's newer linker chokes on the prebuilt Cocoa LCL units shipped
    # with the Lazarus cask ("malformed method list atom ... fixups found
    # beyond the number of method entries" in cocoawsextctrls.o and
    # similar). Force the classic linker to work around it.
    (cd "$REPO_ROOT/source" && "$LAZBUILD" --opt="-k-ld_classic" webgen.lpi)

    if [ -f "$REPO_ROOT/bin/webgen" ]; then
        cp -f "$REPO_ROOT/bin/webgen" "$OUT_DIR/webgen-macos-$(uname -m)"
        print_success "Built: $OUT_DIR/webgen-macos-$(uname -m)"
    else
        print_error "Build finished but output binary was not found."
        exit 1
    fi
}


clean_all() {
    # Deliberately scoped to macOS-only artifacts - NOT `make clean`, which
    # wipes all of build/ (including build/debian, build/alpine from the
    # sibling container scripts).
    print_info "Removing $OUT_DIR..."
    rm -rf "$OUT_DIR"
    print_info "Removing kayte_arm64_emit.o and macOS dylibs..."
    rm -f "$REPO_ROOT/source/kayte_arm64_emit.o"
    rm -f "$REPO_ROOT/lib"/*.dylib
    print_info "Removing lazbuild output binaries (bin/) and units (build/units/)..."
    rm -f "$REPO_ROOT/bin/kayte" "$REPO_ROOT/bin/vb6interpreter" "$REPO_ROOT/bin/webgen"
    rm -rf "$REPO_ROOT/build/units"
    print_success "Clean."
}

case "$COMMAND" in
    build)
        require_host
        ensure_fpc
        ensure_lazbuild
        compile_kayte
        ;;
    vb6)
        require_host
        ensure_fpc
        ensure_lazbuild
        compile_vb6interpreter
        ;;
    webgen)
        require_host
        ensure_fpc
        ensure_lazbuild
        compile_webgen
        ;;
    clean)
        require_host
        clean_all
        ;;
    help)
        cat <<EOF
Usage: $(basename "$0") [command]

Commands:
  build   (default) build the Mach-O native backend lib and compile
          source/kayte.lpi, copying the resulting binary out.
  vb6     same, but for source/vb6interpreter.lpi (an LCL app - on
          macOS this builds against the Cocoa widgetset).
  webgen  same, but for source/webgen.lpi (an LCL app - on
          macOS this builds against the Cocoa widgetset).
  clean   remove build/macos and the intermediate object/unit files
          left behind in the repo.
EOF
        ;;
    *)
        print_error "Unknown command: $COMMAND (expected: build|vb6|webgen|clean|help)"
        exit 1
        ;;
esac
