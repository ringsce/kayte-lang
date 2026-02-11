# Kayte Language - musl Build System Files

## 📦 Complete Package Contents

This package contains everything you need to build the Kayte language with musl libc for Linux ARM64 and AMD64.

### 🚀 Quick Start Files

1. **QUICKSTART_KAYTE.md** - Start here! Quick reference for building
2. **setup_and_build_kayte.sh** - All-in-one setup and build script

### 📖 Documentation

1. **README_KAYTE_MUSL.md** - Complete documentation for musl builds
2. **QUICKSTART_KAYTE.md** - Quick start guide
3. **QUICK_REFERENCE.md** - Command reference card
4. **INDEX.md** - This file

### 🔧 Build Scripts

1. **setup_and_build_kayte.sh** ⭐ Recommended for first-time setup
   - Downloads and installs musl toolchains
   - Configures environment
   - Builds Kayte for both architectures
   - All-in-one solution

2. **build_kayte_musl.sh** - Standalone build script
   - Assumes toolchains are already installed
   - Flexible architecture selection
   - Detailed output and verification

3. **setup_lazarus_musl.sh** - General musl toolchain setup
   - Installs ARM64 and AMD64 musl toolchains
   - Creates environment scripts
   - Useful for any FreePascal project

### 📋 Makefiles

1. **Makefile.kayte** - Complete Makefile for Kayte
   - Multiple build targets
   - Testing support
   - Installation support
   - Distribution package creation

2. **Makefile** - General purpose musl Makefile template
   - Can be adapted for other projects

### ⚙️ Configuration Files

1. **fpc-musl.cfg** - Combined configuration reference
2. **fpc-arm64-musl.cfg** - ARM64-specific configuration
3. **fpc-amd64-musl.cfg** - AMD64-specific configuration

### 📝 Sample & Template Files

1. **sample_musl_project.lpi** - Lazarus project template
2. **TestMusl.pas** - Test program for verifying musl builds

### 📚 General Documentation

1. **README.md** - General guide for Lazarus IDE 4.x with musl
2. **lazarus_musl_setup_guide.md** - Detailed Lazarus configuration guide

## 🎯 Usage Scenarios

### Scenario 1: First Time Setup (Recommended)

```bash
# Make executable
chmod +x setup_and_build_kayte.sh

# Run as root (installs to /opt)
sudo ./setup_and_build_kayte.sh both

# This will:
# 1. Install musl toolchains
# 2. Setup environment
# 3. Build Kayte for ARM64 and AMD64
```

### Scenario 2: Using Pre-installed Toolchains

```bash
# If you already have musl toolchains installed
chmod +x build_kayte_musl.sh

# Build both architectures
./build_kayte_musl.sh both

# Or build specific architecture
./build_kayte_musl.sh arm64
./build_kayte_musl.sh amd64
```

### Scenario 3: Using Make

```bash
# Build both
make -f Makefile.kayte all

# Build and test
make -f Makefile.kayte all test

# Install system-wide
sudo make -f Makefile.kayte install
```

### Scenario 4: Using FreePascal Directly

```bash
# Setup environment first
source ~/.lazarus_musl_env

# Use config files
fpc @fpc-arm64-musl.cfg kayte.lpr
fpc @fpc-amd64-musl.cfg kayte.lpr
```

## 📂 File Organization

Place these files in your Kayte project directory structure:

```
kayte-lang/
├── kayte.lpr                         # Your main program
├── source/                           # Your source files
│   ├── Lexer.pas
│   ├── Parser.pas
│   └── ...
├── components/                       # Components
│   └── http/
│       └── SimpleHTTPServer.pas
│
├── build_kayte_musl.sh              # Build script
├── setup_and_build_kayte.sh         # All-in-one script
├── Makefile.kayte                   # Makefile
│
├── fpc-arm64-musl.cfg               # ARM64 config
├── fpc-amd64-musl.cfg               # AMD64 config
├── fpc-musl.cfg                     # Combined config
│
├── README_KAYTE_MUSL.md             # Documentation
├── QUICKSTART_KAYTE.md              # Quick start
└── INDEX.md                         # This file
```

## 🔍 File Details

### Build Scripts (*.sh)

All shell scripts need to be made executable:
```bash
chmod +x *.sh
```

**setup_and_build_kayte.sh** (7.8 KB)
- Complete setup and build automation
- Requires root/sudo access
- Downloads toolchains if needed
- Builds Kayte automatically

**build_kayte_musl.sh** (10.3 KB)
- Standalone build script
- Assumes toolchains installed
- Detailed output and checks
- Architecture selection

**setup_lazarus_musl.sh** (5.8 KB)
- General toolchain installer
- Creates environment scripts
- Reusable for any project

### Makefiles

**Makefile.kayte** (7.1 KB)
- Kayte-specific Makefile
- Targets: all, arm64, amd64, test, install, clean
- Color output
- Detailed status messages

**Makefile** (4.6 KB)
- Generic template
- Adaptable for other projects
- Standard Make targets

### Configuration Files (*.cfg)

FreePascal configuration files with all necessary compiler options.

**fpc-arm64-musl.cfg** (438 bytes)
```bash
fpc @fpc-arm64-musl.cfg kayte.lpr
```

**fpc-amd64-musl.cfg** (422 bytes)
```bash
fpc @fpc-amd64-musl.cfg kayte.lpr
```

**fpc-musl.cfg** (2.9 KB)
Reference configuration with comments.

### Documentation

**README_KAYTE_MUSL.md** (8.1 KB)
- Complete build guide
- Troubleshooting
- CI/CD integration
- Performance tips

**QUICKSTART_KAYTE.md** (1.7 KB)
- Minimal quick start
- Essential commands only
- Common issues

**QUICK_REFERENCE.md** (2.8 KB)
- Command reference
- Compiler flags
- Quick fixes

### Sample Files

**sample_musl_project.lpi** (3.2 KB)
- Lazarus project template
- Build modes configured
- Ready to use

**TestMusl.pas** (3.5 KB)
- Test FreePascal program
- Verifies musl compilation
- Tests runtime features

## 🎓 Learning Path

1. **Complete Beginner**: Start with `setup_and_build_kayte.sh`
2. **Some Experience**: Use `build_kayte_musl.sh` or `Makefile.kayte`
3. **Advanced User**: Use `fpc` directly with config files
4. **Lazarus IDE User**: See `lazarus_musl_setup_guide.md` and `sample_musl_project.lpi`

## ✅ Verification Checklist

After building, verify your binaries:

```bash
# 1. Check file type
file bin/aarch64-linux-musl/kayte
file bin/x86_64-linux-musl/kayte

# 2. Verify static linking
ldd bin/aarch64-linux-musl/kayte  # Should say "not a dynamic executable"
ldd bin/x86_64-linux-musl/kayte   # Should say "not a dynamic executable"

# 3. Test execution
./bin/x86_64-linux-musl/kayte --version

# 4. Check size
du -h bin/*/kayte
```

## 🆘 Support

If you encounter issues:

1. Check **QUICKSTART_KAYTE.md** for common problems
2. Read **README_KAYTE_MUSL.md** troubleshooting section
3. Verify prerequisites in **QUICK_REFERENCE.md**
4. Check compiler output for specific errors

## 📊 Build Time Estimates

- **First build** (with toolchain download): 10-15 minutes
- **Subsequent builds**: 1-2 minutes
- **Parallel builds** (-j2): ~50% faster

## 🎯 Expected Output

After successful build:

```
bin/
├── aarch64-linux-musl/
│   └── kayte              (~2-4 MB, statically linked)
└── x86_64-linux-musl/
    └── kayte              (~2-4 MB, statically linked)
```

## 📝 Notes

- All scripts require bash (not sh)
- Root access needed for toolchain installation
- FreePascal 3.2.2 or newer recommended
- Ubuntu/Debian tested, other distros may work

## 🔗 Related Files

This package integrates with the general Lazarus musl setup:
- **lazarus_musl_setup_guide.md** - For Lazarus IDE users
- **setup_lazarus_musl.sh** - General toolchain setup
- **README.md** - General FreePascal/Lazarus guide

## 📅 Version

- **Package Version**: 1.0
- **Created**: February 2026
- **Target**: Kayte Language (kayte.lpr)
- **Tested With**: FreePascal 3.2.2, musl 1.2.3

---

**Ready to build? Start with QUICKSTART_KAYTE.md!**
