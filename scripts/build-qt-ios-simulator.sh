#!/usr/bin/env bash
#
# build-qt-ios-simulator.sh
#
# Builds Qt (qtbase, qtshadertools, qtdeclarative - enough for QT widgets
# and QML) for the arm64 iOS Simulator, into ~/Qt/<version>/ios-simulator-arm64.
#
# Why: Qt's prebuilt iOS libraries (aqt install-qt mac ios ...) contain
# arm64 for devices but only x86_64 for the Simulator, and recent iOS
# Simulator runtimes on Apple Silicon no longer run x86_64 apps. With this
# build present, scripts/build-kayte-ios.sh uses it for Simulator builds.
#
# Usage:
#   scripts/build-qt-ios-simulator.sh [version]     (default: 6.11.1)
#
# Needs: Xcode, cmake, ninja, aqt (aqtinstall), and a desktop Qt of the
# same version for the build tools (Homebrew's qt, or ~/Qt/<version>/macos).
# Takes roughly 30-60 minutes and a few GB of disk.

set -euo pipefail

VERSION="${1:-6.11.1}"
SRC_ROOT="$HOME/Qt/src"
SRC="$SRC_ROOT/$VERSION/Src"
BUILD="$SRC_ROOT/build-sim-$VERSION"
PREFIX="$HOME/Qt/$VERSION/ios-simulator-arm64"

die() { echo "[ERROR] $*" >&2; exit 1; }
info() { echo "[INFO] $*"; }

for tool in cmake ninja aqt xcodebuild; do
    command -v "$tool" >/dev/null || die "$tool not found"
done

HOST=""
for candidate in "$HOME/Qt/$VERSION/macos" "$(brew --prefix qt 2>/dev/null || true)"; do
    if [ -n "$candidate" ] && grep -qs "PACKAGE_VERSION \"$VERSION\"" "$candidate"/lib/cmake/Qt6/Qt6ConfigVersion*.cmake; then
        HOST="$candidate"; break
    fi
done
[ -n "$HOST" ] || die "no desktop Qt $VERSION for the build tools (aqt install-qt mac desktop $VERSION -O ~/Qt)"

MODULES=(qtbase qtshadertools qtdeclarative)
if [ ! -d "$SRC/qtdeclarative" ]; then
    info "Downloading Qt $VERSION sources (${MODULES[*]})..."
    mkdir -p "$SRC_ROOT"
    aqt install-src mac "$VERSION" --archives "${MODULES[@]}" -O "$SRC_ROOT"
fi

info "Building qtbase (host tools from $HOST)..."
mkdir -p "$BUILD/qtbase"
(cd "$BUILD/qtbase" \
    && "$SRC/qtbase/configure" -prefix "$PREFIX" -platform macx-ios-clang -sdk iphonesimulator \
        -qt-host-path "$HOST" -release -static -nomake examples -nomake tests -no-feature-sql \
        -- -G Ninja -DCMAKE_OSX_ARCHITECTURES=arm64 \
    && cmake --build . --parallel && cmake --install .)

for module in qtshadertools qtdeclarative; do
    info "Building $module..."
    mkdir -p "$BUILD/$module"
    (cd "$BUILD/$module" \
        && "$PREFIX/bin/qt-configure-module" "$SRC/$module" -- -G Ninja \
        && cmake --build . --parallel && cmake --install .)
done

echo "[OK] Qt $VERSION for the arm64 iOS Simulator: $PREFIX"
