#!/usr/bin/env bash
#
# build-kayte-alpine-container.sh
#
# Installs Apple's `container` CLI (native, lightweight-VM Linux
# containers for Apple silicon Macs — https://github.com/apple/container),
# uses it to build an Alpine Linux image with FreePascal (fpc) and a
# headless Lazarus IDE 4.x toolchain (lazbuild), then builds Kayte
# (source/kayte.lpi) inside that container.
#
# Alpine ships fpc (edge/testing repo) but has no `lazarus` apk package,
# so Lazarus is built from source. Only `lazbuild` itself is built with
# LCL_PLATFORM=nogui (it doesn't render UI) - but the image also carries
# GTK2 dev packages so lazbuild can compile the real LCL package on demand
# for GUI projects like source/vb6interpreter.lpi.
#
# Usage:
#   scripts/build-kayte-alpine-container.sh [command]
#
# Commands:
#   build   (default) install/start the container runtime, build the
#           builder image if missing, compile source/kayte.lpi inside
#           it, and copy the resulting Linux binary out.
#   static  a fully static kayte (build/alpine/kayte-static) for any Linux
#           of its architecture - glibc distributions too; no QT / QML (no dlopen).
#   vb6     same, but for source/vb6interpreter.lpi (a GTK2 LCL app -
#           the produced binary needs a display, X11/Wayland or Xvfb, to
#           actually render forms).
#   image   only (re)build the builder image.
#   shell   drop into an interactive shell inside the builder image,
#           with the repo mounted at /workspace.
#   clean   remove stopped containers from this image, then the image itself.
#   containers  remove stopped containers from this image, without touching
#           the image.
#
# The container this script runs (kayte-alpine-build) is force-removed on
# every exit regardless of outcome - Apple's `container` tool doesn't always
# auto-remove on --rm.
#
# Env overrides:
#   ARCH             arm64 (default) or amd64: x86_64 Linux, built in an
#                    amd64 container that Apple's tool runs through Rosetta
#                    (output in build/alpine-amd64/, image
#                    kayte-alpine-builder-amd64)
#   WITH_GUI         1: put the GTK dev packages in the image (needed
#                    for 'vb6'); the default is 1 for arm64, 0 for amd64
#   ALPINE_VERSION   Alpine base image tag (default: 3.24.1)
#   LAZARUS_REF      git ref of the Lazarus source to build (default: lazarus_4_0)
#   IMAGE_NAME       builder image tag (default: kayte-alpine-builder)

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

ALPINE_VERSION="${ALPINE_VERSION:-3.24.1}"
LAZARUS_REF="${LAZARUS_REF:-lazarus_4_0}"
# ARCH=arm64 (default, native) or amd64 (x86_64, run through Rosetta):
# each has its own builder image and output directory.
ARCH="${ARCH:-arm64}"
case "$ARCH" in
    arm64) OUT_REL="build/alpine"; IMAGE_DEFAULT="kayte-alpine-builder" ;;
    amd64) OUT_REL="build/alpine-amd64"; IMAGE_DEFAULT="kayte-alpine-builder-amd64" ;;
    *) echo "ARCH must be arm64 or amd64 (got $ARCH)" >&2; exit 1 ;;
esac
PLATFORM="linux/$ARCH"
# GTK dev packages, only needed for the GUI project (vb6). Under Rosetta
# (amd64) they're slow to install - apk has been seen to hang on them (on
# icu-libs) - so amd64 leaves them out unless WITH_GUI=1. (kayte itself
# links SDL2 / SDL3, which are always installed.)
if [ "${WITH_GUI:-$([ "$ARCH" = arm64 ] && echo 1 || echo 0)}" = 1 ]; then
    GUI_PACKAGES="gtk+2.0-dev gtk+3.0-dev"
else
    GUI_PACKAGES=""
fi
IMAGE_NAME="${IMAGE_NAME:-$IMAGE_DEFAULT}"
CONTAINER_NAME="kayte-alpine-build-$ARCH"

IMAGE_CTX_DIR="$REPO_ROOT/$OUT_REL/image"
OUT_DIR="$REPO_ROOT/$OUT_REL"

COMMAND="${1:-build}"

# Apple's `container` tool doesn't reliably auto-remove containers on exit
# even with --rm, so the named container this script uses is force-removed
# on every exit (success, failure, or interrupt) to avoid it piling up or
# colliding with the next run.
remove_named_container() {
    container rm -f "$CONTAINER_NAME" >/dev/null 2>&1 || true
}
trap remove_named_container EXIT

require_host() {
    if [ "$(uname -s)" != "Darwin" ]; then
        print_error "This script drives Apple's 'container' tool and only runs on macOS."
        exit 1
    fi
    if [ "$(uname -m)" != "arm64" ]; then
        print_warn "container requires Apple silicon (arm64); $(uname -m) is not supported by Apple's tool."
    fi
}

ensure_container_cli() {
    if command -v container >/dev/null 2>&1; then
        print_success "Apple container CLI found: $(container --version 2>/dev/null || echo present)"
        return
    fi

    print_info "Apple container CLI not found, installing..."
    if command -v brew >/dev/null 2>&1; then
        brew install container
    else
        print_error "Homebrew not found. Install Apple's container CLI manually:"
        print_error "  https://github.com/apple/container/releases (signed .pkg installer)"
        exit 1
    fi

    if ! command -v container >/dev/null 2>&1; then
        print_error "container CLI still not on PATH after install."
        exit 1
    fi
    print_success "container CLI installed."
}

ensure_container_system() {
    print_info "Starting container system (no-op if already running)..."
    if ! container system status >/dev/null 2>&1; then
        container system start
    fi
    print_success "container system is running."
}

write_dockerfile() {
    mkdir -p "$IMAGE_CTX_DIR"
    cat > "$IMAGE_CTX_DIR/Dockerfile" <<EOF
FROM alpine:${ALPINE_VERSION}

# fpc only exists in Alpine's edge/testing repo; pull in edge/main and
# edge/community too so its dependencies resolve cleanly.
RUN { \\
        echo "https://dl-cdn.alpinelinux.org/alpine/edge/main"; \\
        echo "https://dl-cdn.alpinelinux.org/alpine/edge/community"; \\
        echo "https://dl-cdn.alpinelinux.org/alpine/edge/testing"; \\
    } >> /etc/apk/repositories \\
    && apk update \\
    && apk add --no-cache \\
        fpc \\
        build-base \\
        binutils \\
        git \\
        make \\
        bash \\
        tar \\
        ca-certificates \\
        sdl2-dev \\
        sdl3-dev ${GUI_PACKAGES}

# There is no 'lazarus' apk package, so build lazbuild (the headless
# command-line project builder Lazarus IDE ships) straight from source.
# LCL_PLATFORM=nogui skips the gtk2/qt widgetset for lazbuild itself -
# lazbuild doesn't render UI and kayte.lpi is a plain console program.
# GUI projects that require the LCL package (e.g. vb6interpreter.lpi,
# widgetset gtk2) have it compiled on demand by lazbuild via their
# RequiredPackages when they're built.
RUN git clone --depth 1 --branch ${LAZARUS_REF} \\
        https://gitlab.com/freepascal.org/lazarus/lazarus.git /opt/lazarus \\
    && cd /opt/lazarus \\
    && make lazbuild LCL_PLATFORM=nogui FPC=/usr/bin/fpc \\
    && install -Dm755 lazbuild /usr/local/bin/lazbuild \\
    && lazbuild --lazarusdir=/opt/lazarus --version

WORKDIR /workspace
ENTRYPOINT ["/bin/bash"]
EOF
}

build_image() {
    write_dockerfile
    print_info "Building $IMAGE_NAME (alpine:${ALPINE_VERSION} + fpc + lazbuild @ ${LAZARUS_REF})..."
    container build --platform "$PLATFORM" -t "$IMAGE_NAME" -f "$IMAGE_CTX_DIR/Dockerfile" "$IMAGE_CTX_DIR"
    print_success "Image $IMAGE_NAME built."
}

image_exists() {
    # (the JSON listing names images with their tag: "name:latest")
    container image list 2>/dev/null | awk 'NR > 1 { print $1 }' | grep -qx "$IMAGE_NAME"
}

ensure_image() {
    if ! image_exists; then
        build_image
    else
        print_success "Image $IMAGE_NAME already present, skipping build (use 'image' command to force a rebuild)."
    fi
}

compile_kayte() {
    mkdir -p "$OUT_DIR"
    print_info "Building source/kayte.lpi inside $IMAGE_NAME..."
    container run --rm \
        --platform "$PLATFORM" \
        --name "$CONTAINER_NAME" \
        -v "$REPO_ROOT:/workspace" \
        -w /workspace \
        "$IMAGE_NAME" \
        -lc "rm -f lib/libkaytearm64elf.so \
            && make elf \
            && cp -f lib/libkaytearm64elf.so /usr/local/lib/ \
            && cd source \
            && lazbuild --lazarusdir=/opt/lazarus --opt=-o/workspace/$OUT_REL/kayte kayte.lpi \
            && cp -f /workspace/lib/libkaytearm64elf.so /workspace/$OUT_REL/"

    if [ -f "$OUT_DIR/kayte" ]; then
        print_success "Built: $OUT_DIR/kayte"
        print_info "libkaytearm64elf.so copied alongside it — run with:"
        print_info "  LD_LIBRARY_PATH=$OUT_DIR $OUT_DIR/kayte"
        print_info "(the binary also needs libSDL2.so/libSDL3.so on the target system if you use those features)"
    else
        print_error "Build finished but output binary was not found."
        exit 1
    fi
}

# A fully static musl binary: no dynamic loader or libc needed, so it runs
# on any Linux of its architecture (glibc distributions too). libkaytearm64elf is linked
# in as a static archive, and libgcc's directory is passed explicitly
# because fpc calls ld itself. A static musl binary can't dlopen, so QT /
# QML statements report that libkayte_qt6 can't be loaded; everything else
# (the VM, --native, --llvm, files ...) works.
compile_kayte_static() {
    mkdir -p "$OUT_DIR/static"
    print_info "Building a static source/kayte.lpi inside $IMAGE_NAME..."
    container run --rm \
        --platform "$PLATFORM" \
        --name "$CONTAINER_NAME" \
        -v "$REPO_ROOT:/workspace" \
        -w /workspace \
        "$IMAGE_NAME" \
        -lc "set -e \
            && gcc -O2 -Wall -c source/kayte_arm64_elf.c -o $OUT_REL/static/kayte_arm64_elf.o \
            && ar rcs $OUT_REL/static/libkaytearm64elf.a $OUT_REL/static/kayte_arm64_elf.o \
            && cd source \
            && lazbuild --lazarusdir=/opt/lazarus --opt=-Xt --opt=-Fl/workspace/$OUT_REL/static \
                --opt=-Fl\$(dirname \$(gcc -print-libgcc-file-name)) \
                --opt=-o/workspace/$OUT_REL/kayte-static kayte.lpi"

    if [ -f "$OUT_DIR/kayte-static" ]; then
        print_success "Built: $OUT_DIR/kayte-static (statically linked, runs on any Linux $ARCH)"
    else
        print_error "Build finished but output binary was not found."
        exit 1
    fi
}

compile_vb6interpreter() {
    mkdir -p "$OUT_DIR"
    print_info "Building source/vb6interpreter.lpi inside $IMAGE_NAME..."
    container run --rm \
        --platform "$PLATFORM" \
        --name "$CONTAINER_NAME" \
        -v "$REPO_ROOT:/workspace" \
        -w /workspace/source \
        "$IMAGE_NAME" \
        -lc "lazbuild --lazarusdir=/opt/lazarus --opt=-o/workspace/$OUT_REL/vb6interpreter-linux-$ARCH vb6interpreter.lpi"

    if [ -f "$OUT_DIR/vb6interpreter-linux-$ARCH" ]; then
        print_success "Built: $OUT_DIR/vb6interpreter-linux-$ARCH"
        print_info "This is a real GTK2 GUI app - it needs a display (X11/Wayland, or Xvfb for headless testing) to render forms."
    else
        print_error "Build finished but output binary was not found."
        exit 1
    fi
}

run_shell() {
    ensure_container_cli
    ensure_container_system
    ensure_image
    print_info "Dropping into $IMAGE_NAME with the repo mounted at /workspace..."
    container run --rm -it \
        --platform "$PLATFORM" \
        -v "$REPO_ROOT:/workspace" \
        -w /workspace \
        "$IMAGE_NAME" \
        -l
}

remove_stopped_containers() {
    local ids
    ids="$(container list -a 2>/dev/null | awk -v img="$IMAGE_NAME" 'NR>1 && $2 ~ ("^"img"(:|$)") {print $1}')"
    if [ -z "$ids" ]; then
        print_success "No stopped $IMAGE_NAME containers to remove."
        return
    fi
    print_info "Removing stopped $IMAGE_NAME containers..."
    # shellcheck disable=SC2086
    container rm $ids >/dev/null 2>&1 || true
    print_success "Removed stopped containers."
}

clean_image() {
    remove_stopped_containers
    print_info "Removing image $IMAGE_NAME..."
    container image rm "$IMAGE_NAME" 2>/dev/null || print_warn "Image $IMAGE_NAME not present."
    rm -rf "$IMAGE_CTX_DIR"
    print_success "Clean."
}

case "$COMMAND" in
    build)
        require_host
        ensure_container_cli
        ensure_container_system
        ensure_image
        compile_kayte
        ;;
    static)
        require_host
        ensure_container_cli
        ensure_container_system
        ensure_image
        compile_kayte_static
        ;;
    vb6)
        require_host
        ensure_container_cli
        ensure_container_system
        ensure_image
        compile_vb6interpreter
        ;;
    image)
        require_host
        ensure_container_cli
        ensure_container_system
        build_image
        ;;
    shell)
        require_host
        run_shell
        ;;
    clean)
        require_host
        ensure_container_cli
        clean_image
        ;;
    containers)
        require_host
        ensure_container_cli
        remove_stopped_containers
        ;;
    help)
        cat <<EOF
Usage: $(basename "$0") [command]

Commands:
  build   (default) install/start the container runtime, build the
          builder image if missing, compile source/kayte.lpi inside
          it, and copy the resulting Linux binary out.
  static  a fully static kayte (build/alpine/kayte-static) that runs on
          any Linux of its architecture, glibc distributions too (no QT / QML: a static
          binary can't load libkayte_qt6).
  vb6     same, but for source/vb6interpreter.lpi (a GTK2 LCL app -
          the produced binary needs a display, X11/Wayland or Xvfb, to
          actually render forms).
  image   only (re)build the builder image.
  shell   drop into an interactive shell inside the builder image,
          with the repo mounted at /workspace.
  clean   remove stopped containers from this image, then the image itself.
  containers  remove stopped containers from this image, without touching
          the image.

ARCH=amd64 builds for x86_64 instead of ARM64 (through Rosetta), into
build/alpine-amd64/:  ARCH=amd64 $(basename "$0") build
EOF
        ;;
    *)
        print_error "Unknown command: $COMMAND (expected: build|static|vb6|image|shell|clean|containers)"
        exit 1
        ;;
esac
