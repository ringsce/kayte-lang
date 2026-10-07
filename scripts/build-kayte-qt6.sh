#!/usr/bin/env bash
#
# build-kayte-qt6.sh
#
# Builds libkayte_qt6 - the C ABI shim over QtWidgets and Qt Quick (QML)
# that backs Kayte's QT statement (source/qt6/kayte_qt6.cpp, loaded by
# source/kayte_qt6.pas) - and copies it next to any kayte binary found in
# bin/ or build/macos/, where kayte looks for it first.
#
# kayte itself doesn't need rebuilding against Qt: the library is only
# loaded when a script runs its first QT statement.
#
# Usage:
#   scripts/build-kayte-qt6.sh [qt6-prefix]
#
# The Qt6 prefix is optional: by default CMake's normal search is used,
# plus `brew --prefix qt` on macOS when Homebrew is available. Pass it
# explicitly for an installer/aqtinstall Qt, e.g. ~/Qt/6.8.0/macos.
#
# Requirements: cmake >= 3.16, a C++17 compiler, Qt6 (Widgets; Qt Quick
# optional, for QML).
#   macOS:  brew install qt cmake
#   Debian: apt install qt6-base-dev qt6-declarative-dev cmake g++

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
BUILD_DIR="$REPO_ROOT/build/qt6"

QT_PREFIX="${1:-}"
if [ -z "$QT_PREFIX" ] && [ "$(uname -s)" = "Darwin" ] && command -v brew >/dev/null 2>&1; then
    QT_PREFIX="$(brew --prefix qt 2>/dev/null || true)"
fi

CMAKE_ARGS=(-S "$REPO_ROOT/source/qt6" -B "$BUILD_DIR" -DCMAKE_BUILD_TYPE=Release)
if [ -n "$QT_PREFIX" ]; then
    CMAKE_ARGS+=("-DCMAKE_PREFIX_PATH=$QT_PREFIX")
fi

echo "[INFO] Configuring libkayte_qt6${QT_PREFIX:+ (Qt6 prefix: $QT_PREFIX)}..."
cmake "${CMAKE_ARGS[@]}"
cmake --build "$BUILD_DIR" --config Release

case "$(uname -s)" in
    Darwin) LIB_NAME="libkayte_qt6.dylib" ;;
    MINGW*|MSYS*|CYGWIN*) LIB_NAME="kayte_qt6.dll" ;;
    *) LIB_NAME="libkayte_qt6.so" ;;
esac

LIB_PATH="$(find "$BUILD_DIR" -name "$LIB_NAME" -not -path "*/CMakeFiles/*" | head -n 1)"
if [ -z "$LIB_PATH" ]; then
    echo "[ERROR] Build finished but $LIB_NAME was not found under $BUILD_DIR."
    exit 1
fi
echo "[OK] Built: $LIB_PATH"

for dir in "$REPO_ROOT/bin" "$REPO_ROOT/build/macos"; do
    if [ -f "$dir/kayte" ] || [ -f "$dir/kayte.exe" ]; then
        cp -f "$LIB_PATH" "$dir/"
        echo "[OK] Copied next to $dir/kayte"
    fi
done
