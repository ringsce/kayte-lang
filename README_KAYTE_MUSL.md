# Building Kayte Language with musl libc

Complete guide for building the Kayte programming language interpreter/compiler with musl libc for Linux ARM64 and AMD64 architectures.

## 📋 Overview

Kayte is a programming language interpreter and compiler. This guide shows how to build statically-linked, portable binaries using musl libc instead of glibc.

**Benefits of musl builds:**
- ✅ Statically linked - no runtime dependencies
- ✅ Smaller binary sizes
- ✅ Maximum portability across Linux distributions
- ✅ Consistent behavior across different systems

## 🚀 Quick Start

### 1. Install musl Toolchains

```bash
# Run the setup script from the parent directory
chmod +x setup_lazarus_musl.sh
sudo ./setup_lazarus_musl.sh
```

This installs:
- ARM64 musl toolchain → `/opt/aarch64-linux-musl-cross`
- AMD64 musl toolchain → `/opt/x86_64-linux-musl-cross`

### 2. Build Kayte

#### Using the Build Script (Recommended):

```bash
# Make the script executable
chmod +x build_kayte_musl.sh

# Build for both architectures
./build_kayte_musl.sh both

# Or build for specific architecture
./build_kayte_musl.sh arm64
./build_kayte_musl.sh amd64
```

#### Using Make:

```bash
# Build for both architectures
make -f Makefile.kayte all

# Build for specific architecture
make -f Makefile.kayte arm64
make -f Makefile.kayte amd64

# Quick parallel build
make -f Makefile.kayte quick
```

## 📂 Project Structure

```
kayte-lang/
├── kayte.lpr                    # Main program file
├── build_kayte_musl.sh          # Build script
├── Makefile.kayte               # Makefile for musl builds
├── bin/                         # Output binaries
│   ├── aarch64-linux-musl/
│   │   └── kayte                # ARM64 binary
│   └── x86_64-linux-musl/
│       └── kayte                # AMD64 binary
├── lib/                         # Compiled units
│   ├── aarch64-linux-musl/
│   └── x86_64-linux-musl/
└── source/                      # Source files
    ├── Lexer.pas
    ├── Parser.pas
    ├── Compiler.pas
    ├── VirtualMachine.pas
    └── ...
```

## 🔧 Build Options

### Build Targets

| Command | Description |
|---------|-------------|
| `./build_kayte_musl.sh both` | Build for ARM64 and AMD64 |
| `./build_kayte_musl.sh arm64` | Build for ARM64 only |
| `./build_kayte_musl.sh amd64` | Build for AMD64 only |
| `./build_kayte_musl.sh clean` | Clean build artifacts |
| `make -f Makefile.kayte all` | Build both with Make |
| `make -f Makefile.kayte test` | Test binaries |
| `make -f Makefile.kayte install` | Install to /usr/local/bin |

### Compiler Flags Used

**ARM64 musl:**
```bash
-Tlinux                                     # Target OS: Linux
-Paarch64                                   # Target CPU: ARM64
-XPaarch64-linux-musl-                      # Binutils prefix
-Xd                                         # Don't use standard lib path
-Fl/opt/aarch64-linux-musl-cross/.../lib    # Library path
-k-static                                   # Static linking
-O3                                         # Optimization level 3
-CX -XX                                     # Smart linking
-Xs                                         # Strip symbols
```

**AMD64 musl:**
```bash
-Tlinux                                     # Target OS: Linux
-Px86_64                                    # Target CPU: AMD64
-XPx86_64-linux-musl-                       # Binutils prefix
-Xd                                         # Don't use standard lib path
-Fl/opt/x86_64-linux-musl-cross/.../lib     # Library path
-k-static                                   # Static linking
-O3                                         # Optimization level 3
-CX -XX                                     # Smart linking
-Xs                                         # Strip symbols
```

## 🧪 Testing Your Build

### Verify Binary Type

```bash
# Check ARM64 binary
file bin/aarch64-linux-musl/kayte
# Output: ELF 64-bit LSB executable, ARM aarch64, ... statically linked

# Check AMD64 binary
file bin/x86_64-linux-musl/kayte
# Output: ELF 64-bit LSB executable, x86-64, ... statically linked
```

### Verify Static Linking

```bash
# Should output "not a dynamic executable"
ldd bin/aarch64-linux-musl/kayte
ldd bin/x86_64-linux-musl/kayte
```

### Test Execution

```bash
# Test AMD64 (on x86_64 system)
./bin/x86_64-linux-musl/kayte --version

# Test ARM64 with QEMU (on any system)
sudo apt-get install qemu-user-static
qemu-aarch64-static ./bin/aarch64-linux-musl/kayte --version
```

### Run the Test Suite

```bash
# Using Make
make -f Makefile.kayte test
```

## 📦 Dependencies

The Kayte project has several FreePascal unit dependencies:

**Core Compiler Units:**
- Lexer.pas
- Parser.pas
- TokenDefs.pas
- AST.pas
- Compiler.pas
- Assembler.pas
- BytecodeTypes.pas

**VM and Runtime:**
- VirtualMachine.pas
- Bytecode.pas
- kayte_vm.pas
- kayte_runtime.pas

**Native Compilation (ARM64):**
- KayteArm64.pas (macOS)
- KayteArm64ELF.pas (Linux)
- KayteArm64PE.pas (Windows)

**Other Units:**
- SimpleHTTPServer.pas
- XMLParser.pas
- SDK components
- C99 backend support

All unit paths are configured automatically by the build scripts.

## 🎯 Platform-Specific Features

### Linux ARM64 Build

The ARM64 build includes native compilation support via `KayteArm64ELF.pas`:

```pascal
{$IFDEF LINUX}
, KayteArm64ELF in '../source/KayteArm64ELF.pas'
{$ENDIF}
```

This enables Kayte to compile to native ARM64 ELF binaries on Linux.

### Linux AMD64 Build

The AMD64 build provides:
- Bytecode compilation and execution
- VM-based interpretation
- HTTP server functionality
- REPL (Read-Eval-Print Loop)

## 🔍 Troubleshooting

### Common Issues

#### "Can't find unit System"

**Solution:**
```bash
# Install FPC source
sudo apt-get install fp-source

# Or set FPCDIR
export FPCDIR=/usr/share/fpcsrc
```

#### "Assembler not found"

**Solution:**
```bash
# Ensure musl toolchains are in PATH
export PATH="/opt/aarch64-linux-musl-cross/bin:$PATH"
export PATH="/opt/x86_64-linux-musl-cross/bin:$PATH"
```

#### "Can't find unit Lexer/Parser/etc"

**Solution:**
The build scripts use relative paths. Ensure you run them from the directory containing `kayte.lpr`.

#### Linking Errors

**Solution:**
```bash
# Verify library paths
ls -la /opt/aarch64-linux-musl-cross/aarch64-linux-musl/lib/
ls -la /opt/x86_64-linux-musl-cross/x86_64-linux-musl/lib/
```

### Debug Build

For debugging issues, use the debug target:

```bash
make -f Makefile.kayte debug
```

This creates a debug build with:
- Debug symbols (`-g`)
- Line info (`-gl`)
- Heap trace (`-gh`)
- Range checks (`-Cr`)
- I/O checks (`-Ci`)
- Overflow checks (`-Co`)

## 📊 Binary Size Comparison

Typical binary sizes with musl:

| Architecture | Binary Size | Notes |
|--------------|-------------|-------|
| ARM64 musl   | ~2-4 MB     | Statically linked, stripped |
| AMD64 musl   | ~2-4 MB     | Statically linked, stripped |

Compared to glibc builds:
- musl binaries are typically 20-30% smaller
- No runtime library dependencies
- More portable across distributions

## 🚢 Deployment

### Copy Binaries Directly

The musl builds are self-contained:

```bash
# Copy to target system (no dependencies needed)
scp bin/x86_64-linux-musl/kayte user@server:/usr/local/bin/
ssh user@server chmod +x /usr/local/bin/kayte
```

### Create Distribution Package

```bash
# Using Make
make -f Makefile.kayte dist

# Creates:
# dist/kayte-arm64-musl-YYYYMMDD.tar.gz
# dist/kayte-amd64-musl-YYYYMMDD.tar.gz
```

### Install System-Wide

```bash
sudo make -f Makefile.kayte install
```

This installs:
- `/usr/local/bin/kayte-arm64` (ARM64 binary)
- `/usr/local/bin/kayte-amd64` (AMD64 binary)

## 💡 Performance Tips

### Optimization Flags

The build scripts use `-O3` by default. For different optimization levels:

```bash
# Edit Makefile.kayte or build_kayte_musl.sh
# Change -O3 to:
-O2  # Good balance of speed and size
-O3  # Maximum optimization (default)
-O4  # Aggressive optimization (may increase size)
```

### Link-Time Optimization

For even better optimization:

```bash
# Add to compiler options
-Xs      # Strip symbols (already enabled)
-XX      # Smart linking (already enabled)
```

## 🔗 Integration with CI/CD

### GitHub Actions Example

```yaml
name: Build Kayte with musl

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      
      - name: Install FreePascal
        run: sudo apt-get install -y fpc
      
      - name: Setup musl toolchains
        run: |
          chmod +x setup_lazarus_musl.sh
          sudo ./setup_lazarus_musl.sh
      
      - name: Build Kayte
        run: |
          chmod +x build_kayte_musl.sh
          ./build_kayte_musl.sh both
      
      - name: Test binaries
        run: make -f Makefile.kayte test
      
      - name: Upload artifacts
        uses: actions/upload-artifact@v2
        with:
          name: kayte-musl-binaries
          path: bin/
```

### GitLab CI Example

```yaml
build:musl:
  image: ubuntu:22.04
  before_script:
    - apt-get update && apt-get install -y fpc wget
    - ./setup_lazarus_musl.sh
  script:
    - ./build_kayte_musl.sh both
    - make -f Makefile.kayte test
  artifacts:
    paths:
      - bin/
```

## 📚 Additional Resources

- [Kayte Language Documentation](https://github.com/ringsce/kayte-lang)
- [FreePascal Documentation](https://www.freepascal.org/docs.html)
- [musl libc](https://musl.libc.org/)
- [Linux ARM64 ABI](https://developer.arm.com/documentation/)

## 🤝 Contributing

To contribute improvements to the build system:

1. Test your changes with both ARM64 and AMD64 builds
2. Ensure static linking is preserved
3. Update this README with any new requirements
4. Submit a pull request

## 📝 License

The build scripts and configuration are provided as examples for the Kayte project. Refer to the main Kayte project license for terms.

## ✨ Credits

- Kayte Language by Pedro Dias Vicente
- Build system for musl cross-compilation
- FreePascal compiler team
- musl libc team

---

**Build Status:** Tested with FreePascal 3.2.2 and musl 1.2.3

**Last Updated:** February 2026
