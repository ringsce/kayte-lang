#!/usr/bin/env bash
#
# build-kayte-windows.sh
#
# Cross-compiles kayte for Windows from macOS (or Linux): kayte.exe with
# FPC's Windows cross-compiler, kaytearm64pe.dll (the PE backend of
# --native-arm64, which kayte.exe imports) with llvm-mingw, and the C
# runtime next to them so kayte.exe --native / --llvm find it.
#
# Usage:
#   scripts/build-kayte-windows.sh [arm64|x86_64]     (default: arm64)
#
# Output: build/windows-<arch>/{kayte.exe, kaytearm64pe.dll, kayte_native_rt.c}
#
# Requirements:
#   - fpc (3.3.1 from fpcupdeluxe) with Windows units for the target in
#     $FPCUP/fpc/units/<cpu>-win64, built by that same compiler (PPU
#     versions must match). For arm64, the user ~/.fpc.cfg already points
#     the WIN64 target at the aarch64-win64 units and fpcupdeluxe's
#     llvm-mingw (assembler / linker); for x86_64 FPC's internal linker is
#     used and the unit paths are passed here.
#   - llvm-mingw (https://github.com/mstorsjo/llvm-mingw) for the DLL:
#     $LLVM_MINGW, else ~/llvm-mingw, else $FPCUP/cross/llvm-mingw.
#
# Env overrides: FPC (fpc), FPCUP (~/fpcupdeluxe), LLVM_MINGW.

set -euo pipefail

ARCH="${1:-arm64}"
case "$ARCH" in
    arm64|aarch64) CPU=aarch64; TRIPLE=aarch64-w64-mingw32; ARCH=arm64 ;;
    x86_64|amd64|x64) CPU=x86_64; TRIPLE=x86_64-w64-mingw32; ARCH=x86_64 ;;
    *) echo "usage: $(basename "$0") [arm64|x86_64]" >&2; exit 1 ;;
esac

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
FPC="${FPC:-fpc}"
FPCUP="${FPCUP:-$HOME/fpcupdeluxe}"
if [ -z "${LLVM_MINGW:-}" ]; then
    for d in "$HOME/llvm-mingw" "$FPCUP/cross/llvm-mingw"; do
        [ -x "$d/bin/$TRIPLE-clang" ] && { LLVM_MINGW="$d"; break; }
    done
fi
[ -n "${LLVM_MINGW:-}" ] || { echo "[ERROR] llvm-mingw not found: set LLVM_MINGW" >&2; exit 1; }

OUT="$REPO_ROOT/build/windows-$ARCH"
UNITS_OUT="$REPO_ROOT/build/units/kayte/$CPU-win64"
WIN_UNITS="$FPCUP/fpc/units/$CPU-win64"
mkdir -p "$OUT" "$UNITS_OUT"

FPC_ARGS=(-Twin64 -P"$CPU" -Mobjfpc -Scghi -O2 -XX -CX -Fu. -Fu../jvm
          -FE"$OUT" -FU"$UNITS_OUT" -o"$OUT/kayte.exe")
if [ "$CPU" = x86_64 ]; then
    # Skip ~/.fpc.cfg (its WIN64 section is for aarch64) and name the units.
    FPC_ARGS=(-n -Fu"$WIN_UNITS/*" -Fu"$WIN_UNITS/rtl" "${FPC_ARGS[@]}")
fi

echo "[INFO] kayte.exe for Windows $ARCH ($FPC)..."
(cd "$REPO_ROOT/source" && "$FPC" "${FPC_ARGS[@]}" kayte.lpr) || {
    echo "[ERROR] fpc failed. \"Can't find unit system\" means $WIN_UNITS is missing" >&2
    echo "        or was built by another compiler version (PPU mismatch): rebuild the" >&2
    echo "        Windows $CPU cross RTL / packages with this compiler (fpcupdeluxe)." >&2
    exit 1
}

echo "[INFO] kaytearm64pe.dll ($LLVM_MINGW)..."
"$LLVM_MINGW/bin/$TRIPLE-clang" -O2 -Wall -shared \
    -o "$OUT/kaytearm64pe.dll" "$REPO_ROOT/source/kayte_arm64_pe.c"

# kayte.exe --native / --llvm look for the runtime next to the executable.
cp -f "$REPO_ROOT/source/native/kayte_native_rt.c" "$OUT/"

echo "[OK] $OUT:"
ls -1 "$OUT"
