# Quick Start: Building Kayte with musl

## One-Time Setup

```bash
# 1. Install musl toolchains
chmod +x setup_lazarus_musl.sh
sudo ./setup_lazarus_musl.sh

# 2. Add to your PATH (or add to ~/.bashrc)
source ~/.lazarus_musl_env
```

## Build Kayte

### Method 1: Using Build Script (Easiest)

```bash
chmod +x build_kayte_musl.sh

# Build both ARM64 and AMD64
./build_kayte_musl.sh both

# Or build specific architecture
./build_kayte_musl.sh arm64
./build_kayte_musl.sh amd64
```

### Method 2: Using Make

```bash
# Build both
make -f Makefile.kayte all

# Build specific
make -f Makefile.kayte arm64
make -f Makefile.kayte amd64

# Test
make -f Makefile.kayte test
```

### Method 3: Using fpc Directly

```bash
# ARM64
fpc @fpc-arm64-musl.cfg kayte.lpr

# AMD64
fpc @fpc-amd64-musl.cfg kayte.lpr
```

### Method 4: Manual Compilation

```bash
# ARM64
export PATH="/opt/aarch64-linux-musl-cross/bin:$PATH"
fpc -Tlinux -Paarch64 -XPaarch64-linux-musl- -Xd \
    -Fl/opt/aarch64-linux-musl-cross/aarch64-linux-musl/lib \
    -FUlib/aarch64-linux-musl -FEbin/aarch64-linux-musl \
    -k-static -O3 -CX -XX -Xs kayte.lpr

# AMD64
export PATH="/opt/x86_64-linux-musl-cross/bin:$PATH"
fpc -Tlinux -Px86_64 -XPx86_64-linux-musl- -Xd \
    -Fl/opt/x86_64-linux-musl-cross/x86_64-linux-musl/lib \
    -FUlib/x86_64-linux-musl -FEbin/x86_64-linux-musl \
    -k-static -O3 -CX -XX -Xs kayte.lpr
```

## Test Your Build

```bash
# Verify static linking
ldd bin/x86_64-linux-musl/kayte
# Should output: "not a dynamic executable"

# Test AMD64
./bin/x86_64-linux-musl/kayte --version

# Test ARM64 (with QEMU)
qemu-aarch64-static ./bin/aarch64-linux-musl/kayte --version
```

## Output Location

Binaries are created in:
- `bin/aarch64-linux-musl/kayte` (ARM64)
- `bin/x86_64-linux-musl/kayte` (AMD64)

## Common Issues

**"Can't find unit System"**
```bash
sudo apt-get install fp-source
```

**"Assembler not found"**
```bash
source ~/.lazarus_musl_env
```

## Install System-Wide

```bash
sudo make -f Makefile.kayte install
# Installs to /usr/local/bin/kayte-{arm64,amd64}
```

## That's It!

You now have:
- ✅ Statically linked binaries
- ✅ No runtime dependencies
- ✅ Portable across all Linux distributions
- ✅ Smaller binary size vs glibc

For detailed documentation, see `README_KAYTE_MUSL.md`
