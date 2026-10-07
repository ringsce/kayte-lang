#!/usr/bin/env bash
#
# build-kayte-ios.sh
#
# Builds a Kayte program - QT widgets and QML included - into an iOS app.
#
#   1. `kayte --native app.kayte -o app.c` translates the program to C.
#   2. source/ios/CMakeLists.txt compiles that C, the native runtime and
#      the Qt shim (linked in statically: iOS apps can't load libraries
#      from outside their bundle) against Qt for iOS, with Xcode.
#   3. Files given with --resource are bundled at the same relative path,
#      and the program starts in the bundle, so relative paths in the
#      script (QML "load", "ui/main.qml") work as they do on the desktop.
#
# Usage:
#   scripts/build-kayte-ios.sh <program.kayte|program.kjs> [options]
#
# Options:
#   --resource <path>  file or directory to bundle, relative to the current
#                      directory (repeatable), e.g. --resource examples/qml
#   --name <name>      app name (default: the program's file name)
#   --bundle-id <id>   bundle identifier (default: org.kayte.<name>)
#   --device           build for iPhone/iPad hardware (default: Simulator)
#   --team <id>        Apple development team for device signing
#   --qt <dir>         Qt for iOS (default: newest ~/Qt/<version>/ios; for
#                      the Simulator, ~/Qt/<version>/ios-simulator-arm64
#                      next to it when present)
#   --run              install and launch in the Simulator (boots one if
#                      none is running), streaming the program's output
#   -o <dir>           where to put the .app (default: build/ios)
#
# Requirements: Xcode, cmake, a built kayte (bin/kayte or
# build/macos/kayte), and Qt for iOS - the same Qt version as a desktop Qt
# on this Mac (Homebrew's qt works), which provides its build tools:
#   aqt install-qt mac ios 6.11.1 -O ~/Qt      # pip/brew install aqtinstall
#
# Qt's prebuilt iOS libraries only have x86_64 for the Simulator, which
# recent Simulator runtimes on Apple Silicon refuse to run; build an arm64
# Simulator Qt once with scripts/build-qt-ios-simulator.sh.
#
# PROCESS isn't available on iOS (apps can't start other programs).
# tvOS isn't supported: Qt 6 has no tvOS port.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

die() { echo "[ERROR] $*" >&2; exit 1; }
info() { echo "[INFO] $*"; }

PROGRAM=""
RESOURCES=()
NAME=""
BUNDLE_ID=""
DEVICE=0
TEAM=""
QT_IOS=""
RUN=0
OUT_DIR="$REPO_ROOT/build/ios"

while [ $# -gt 0 ]; do
    case "$1" in
        --resource) RESOURCES+=("$2"); shift 2 ;;
        --name) NAME="$2"; shift 2 ;;
        --bundle-id) BUNDLE_ID="$2"; shift 2 ;;
        --device) DEVICE=1; shift ;;
        --simulator) DEVICE=0; shift ;;
        --team) TEAM="$2"; shift 2 ;;
        --qt) QT_IOS="$2"; shift 2 ;;
        --run) RUN=1; shift ;;
        -o) OUT_DIR="$2"; shift 2 ;;
        -h|--help) sed -n '3,38p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
        -*) die "unknown option $1 (see --help)" ;;
        *) [ -z "$PROGRAM" ] || die "only one program can be given"; PROGRAM="$1"; shift ;;
    esac
done

[ -n "$PROGRAM" ] || die "usage: $0 <program.kayte> [options] (see --help)"
[ -f "$PROGRAM" ] || die "no such file: $PROGRAM"
[ "$RUN" = 0 ] || [ "$DEVICE" = 0 ] || die "--run only works with the Simulator"

# --- Tools ---
KAYTE=""
for candidate in "$REPO_ROOT/bin/kayte" "$REPO_ROOT/build/macos/kayte"; do
    if [ -x "$candidate" ]; then KAYTE="$candidate"; break; fi
done
[ -n "$KAYTE" ] || die "kayte not found - build it first (scripts/build-kayte-macos.sh)"
command -v cmake >/dev/null || die "cmake not found (brew install cmake)"
command -v xcodebuild >/dev/null || die "Xcode not found"

SIM_ARCH=x86_64
if [ -z "$QT_IOS" ]; then
    QT_IOS="$(ls -d "$HOME"/Qt/*/ios 2>/dev/null | sort -V | tail -n 1 || true)"
    if [ "$DEVICE" = 0 ] && [ -n "$QT_IOS" ] && [ -x "$(dirname "$QT_IOS")/ios-simulator-arm64/bin/qt-cmake" ]; then
        QT_IOS="$(dirname "$QT_IOS")/ios-simulator-arm64"
    fi
fi
case "$QT_IOS" in *simulator-arm64*) SIM_ARCH=arm64 ;; esac
if [ "$DEVICE" = 0 ] && [ "$SIM_ARCH" = x86_64 ] && [ "$(uname -m)" = arm64 ]; then
    echo "[WARN] Building an x86_64 Simulator app (Qt's prebuilt Simulator libraries); recent"
    echo "       Simulators on Apple Silicon won't run it - see scripts/build-qt-ios-simulator.sh"
fi
[ -x "$QT_IOS/bin/qt-cmake" ] || die "Qt for iOS not found${QT_IOS:+ at $QT_IOS} - install it with:
    aqt install-qt mac ios <version> -O ~/Qt
  (same version as your desktop Qt) or pass --qt <dir>"
QT_VERSION="$(basename "$(dirname "$QT_IOS")")"

# Cross-building Qt apps needs a desktop Qt of the same version for its
# build tools (moc, qmlimportscanner, ...).
QT_HOST=""
for candidate in "$(dirname "$QT_IOS")/macos" "$(brew --prefix qt 2>/dev/null || true)"; do
    if [ -n "$candidate" ] && [ -d "$candidate/lib/cmake/Qt6" ] \
        && grep -qs "PACKAGE_VERSION \"$QT_VERSION\"" "$candidate"/lib/cmake/Qt6/Qt6ConfigVersion*.cmake; then
        QT_HOST="$candidate"; break
    fi
done
[ -n "$QT_HOST" ] || die "no desktop Qt $QT_VERSION found for the build tools - install it
  (aqt install-qt mac desktop $QT_VERSION -O ~/Qt) or a matching Homebrew qt"

# --- Names and paths ---
BASE="$(basename "$PROGRAM")"
BASE="${BASE%.*}"  # app.kayte or app.kjs -> app
[ -n "$NAME" ] || NAME="$(echo "$BASE" | tr -cd 'A-Za-z0-9_-')"
[ -n "$NAME" ] || NAME="KayteApp"
[ -n "$BUNDLE_ID" ] || BUNDLE_ID="org.kayte.$(echo "$NAME" | tr -cd 'A-Za-z0-9-')"
if [ "$DEVICE" = 1 ]; then SDK=iphoneos; else SDK=iphonesimulator; fi

WORK="$REPO_ROOT/build/ios-work/$NAME-$SDK"
STAGE="$WORK/bundle"
rm -rf "$STAGE"
mkdir -p "$STAGE" "$OUT_DIR"

# --- 1. Kayte -> C ---
info "Translating $PROGRAM to C..."
"$KAYTE" --native "$PROGRAM" -o "$WORK/$NAME.c" >"$WORK/kayte.log" 2>&1 \
    || { cat "$WORK/kayte.log"; die "kayte could not compile $PROGRAM"; }

# --- Resources ---
for res in "${RESOURCES[@]+"${RESOURCES[@]}"}"; do
    [ -e "$res" ] || die "resource not found: $res"
    case "$res" in /*|../*|*/../*) die "--resource paths must be inside the current directory: $res" ;; esac
    mkdir -p "$STAGE/$(dirname "$res")"
    cp -R "$res" "$STAGE/$(dirname "$res")/"
done
# Inline QML (QML "source") isn't visible to Qt's import scanner, so make
# sure the common modules are always linked in.
cat > "$STAGE/kayte_qml_imports.qml" <<'QML'
// Generated by build-kayte-ios.sh: makes the static build link these QML
// modules, which inline QML (QML "source") may import.
import QtQuick
import QtQuick.Window
import QtQuick.Controls
import QtQuick.Layouts
Item {}
QML

# --- 2. Build ---
CMAKE_ARGS=(
    -S "$REPO_ROOT/source/ios" -B "$WORK/cmake" -G Xcode
    -DQT_HOST_PATH="$QT_HOST"
    -DKAYTE_PROGRAM_C="$WORK/$NAME.c"
    -DKAYTE_APP_NAME="$NAME"
    -DKAYTE_BUNDLE_ID="$BUNDLE_ID"
    -DKAYTE_STAGE_DIR="$STAGE"
)
if [ "$DEVICE" = 1 ] && [ -n "$TEAM" ]; then
    CMAKE_ARGS+=(-DCMAKE_XCODE_ATTRIBUTE_DEVELOPMENT_TEAM="$TEAM")
fi

# A build directory configured against another Qt can't be reused.
if [ -f "$WORK/cmake/CMakeCache.txt" ] && ! grep -q "^Qt6_DIR:PATH=$QT_IOS/" "$WORK/cmake/CMakeCache.txt"; then
    rm -rf "$WORK/cmake"
fi

info "Configuring with Qt $QT_VERSION for iOS ($SDK, $QT_IOS)..."
"$QT_IOS/bin/qt-cmake" "${CMAKE_ARGS[@]}" >"$WORK/configure.log" 2>&1 \
    || { tail -n 40 "$WORK/configure.log"; die "CMake configure failed (full log: $WORK/configure.log)"; }

info "Building (log: $WORK/build.log)..."
BUILD_ARGS=(-sdk "$SDK")
[ "$DEVICE" = 1 ] && [ -z "$TEAM" ] && BUILD_ARGS+=(CODE_SIGNING_ALLOWED=NO)
[ "$DEVICE" = 0 ] && BUILD_ARGS+=(-arch "$SIM_ARCH")
cmake --build "$WORK/cmake" --config Release -- "${BUILD_ARGS[@]}" >"$WORK/build.log" 2>&1 \
    || { grep -E "error|Error" "$WORK/build.log" | head -n 30; die "build failed (full log: $WORK/build.log)"; }

APP="$(find "$WORK/cmake" -name "$NAME.app" -type d -path "*Release-$SDK*" | head -n 1)"
[ -n "$APP" ] || die "build finished but $NAME.app was not found under $WORK/cmake"
rm -rf "$OUT_DIR/$NAME-$SDK.app"
cp -R "$APP" "$OUT_DIR/$NAME-$SDK.app"
APP="$OUT_DIR/$NAME-$SDK.app"
echo "[OK] Built: $APP"
if [ "$DEVICE" = 1 ] && [ -z "$TEAM" ]; then
    echo "[INFO] Unsigned: pass --team <id> to sign it for a device."
fi

# --- 3. Run in the Simulator ---
if [ "$RUN" = 1 ]; then
    UDID="$(xcrun simctl list devices booted | grep -Eo '[0-9A-F-]{36}' | head -n 1 || true)"
    if [ -z "$UDID" ]; then
        UDID="$(xcrun simctl list devices available | grep -E 'iPhone|iPad' | grep -Eo '[0-9A-F-]{36}' | head -n 1 || true)"
        [ -n "$UDID" ] || die "no iOS Simulator available - add one in Xcode (Settings > Components)"
        info "Booting Simulator $UDID..."
        xcrun simctl boot "$UDID"
        # Show it; the app runs either way (headless if this fails).
        open "$(xcode-select -p)/Applications/Simulator.app" 2>/dev/null || true
    fi
    xcrun simctl install "$UDID" "$APP"
    info "Launching $BUNDLE_ID (Ctrl+C stops following its output)..."
    # Drop harmless system noise so only the program's output shows: two
    # lines every app prints on the iOS 26 Simulator (its accessibility
    # bundles), and Qt's iOS accessibility warning about null elements.
    # simctl only writes line by line to a terminal, so when run from one,
    # `script` gives it one; through a plain pipe its output would sit in a
    # buffer (the program's own PRINT lines are flushed either way).
    LAUNCH=(xcrun simctl launch --console-pty --terminate-running-process "$UDID" "$BUNDLE_ID")
    if [ -t 0 ]; then LAUNCH=(script -q /dev/null "${LAUNCH[@]}"); fi
    "${LAUNCH[@]}" 2>&1 \
        | grep --line-buffered -vE 'NSMapGet\(NSMapTable|^objc\[[0-9]+\]: Class .* is implemented in both|^Could not create a11y element for QAccessibleInterface\(0x0\)' || true
fi
