# Kayte Language ARM64 Native Compiler Makefile
# Supports: macOS (Mach-O), Linux (ELF), Windows (PE)

# Detect OS
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

# Compiler settings
FPC := fpc
GCC := gcc
CLANG := clang

# Directories
SRC_DIR := source
BUILD_DIR := build
LIB_DIR := lib
BIN_DIR := bin

# Compiler flags
FPC_FLAGS := -MObjFPC -Scghi -O3 -g -gl -l -vewnhibq
GCC_FLAGS := -O3 -Wall -fPIC -shared
CLANG_FLAGS := -O3 -Wall -fPIC -shared

# Platform-specific settings
ifeq ($(UNAME_S),Darwin)
    OS := macos
    ARCH := $(UNAME_M)
    LIB_EXT := .dylib
    EXE_EXT :=
    CC := $(CLANG)
    CC_FLAGS := $(CLANG_FLAGS)
    ifeq ($(ARCH),arm64)
        NATIVE_SUPPORT := yes
        TARGET_ARCH := aarch64
    else
        NATIVE_SUPPORT := no
    endif
else ifeq ($(UNAME_S),Linux)
    OS := linux
    ARCH := $(UNAME_M)
    LIB_EXT := .so
    EXE_EXT :=
    CC := $(GCC)
    CC_FLAGS := $(GCC_FLAGS)
    ifeq ($(ARCH),aarch64)
        NATIVE_SUPPORT := yes
        TARGET_ARCH := aarch64
    else
        NATIVE_SUPPORT := no
    endif
else
    # Windows detection (via Git Bash, WSL, or MinGW)
    OS := windows
    LIB_EXT := .dll
    EXE_EXT := .exe
    CC := $(GCC)
    CC_FLAGS := $(GCC_FLAGS)
    NATIVE_SUPPORT := no
endif

# Library names
LIB_MACHO := $(LIB_DIR)/libkaytearm64macho$(LIB_EXT)
LIB_ELF := $(LIB_DIR)/libkaytearm64elf$(LIB_EXT)
LIB_PE := $(LIB_DIR)/libkaytearm64pe$(LIB_EXT)

# Source files
SRC_MACHO := $(SRC_DIR)/kayte_arm64_emit.c
SRC_ELF := $(SRC_DIR)/kayte_arm64_elf.c
SRC_PE := $(SRC_DIR)/kayte_arm64_pe.c

# Pascal units
UNIT_MACHO := $(SRC_DIR)/KayteArm64.pas
UNIT_ELF := $(SRC_DIR)/KayteArm64ELF.pas
UNIT_PE := $(SRC_DIR)/KayteArm64PE.pas

# Object files
OBJ_MACHO := $(BUILD_DIR)/kayte_arm64_macho.o
OBJ_ELF := $(BUILD_DIR)/kayte_arm64_elf.o
OBJ_PE := $(BUILD_DIR)/kayte_arm64_pe.o

# Cross-compilation tools
CROSS_GCC_LINUX := aarch64-linux-gnu-gcc
CROSS_GCC_WIN := aarch64-w64-mingw32-gcc

.PHONY: all clean help native macho elf pe test cross-linux cross-windows info

# Default target
all: info native

# Show build information
info:
	@echo "======================================"
	@echo "Kayte ARM64 Native Compiler Build"
	@echo "======================================"
	@echo "OS:               $(OS)"
	@echo "Architecture:     $(ARCH)"
	@echo "Native Support:   $(NATIVE_SUPPORT)"
	@echo "FPC:              $(shell which $(FPC) 2>/dev/null || echo 'not found')"
	@echo "CC:               $(CC)"
	@echo "======================================"
	@echo ""

# Build native support for current platform
native:
ifeq ($(NATIVE_SUPPORT),yes)
ifeq ($(OS),macos)
	@echo "Building macOS Mach-O support..."
	@$(MAKE) macho
else ifeq ($(OS),linux)
	@echo "Building Linux ELF support..."
	@$(MAKE) elf
endif
else
	@echo "Warning: ARM64 native compilation not supported on $(OS)/$(ARCH)"
	@echo "Only bytecode compilation will be available."
endif

# Build macOS Mach-O support
macho: $(LIB_DIR) $(BUILD_DIR) $(LIB_MACHO)
	@echo "✓ Mach-O support built successfully"

$(LIB_MACHO): $(SRC_MACHO)
	@echo "Compiling Mach-O C backend..."
	$(CC) $(CC_FLAGS) -arch arm64 \
		-framework CoreFoundation \
		-o $(LIB_MACHO) $(SRC_MACHO)
	@echo "✓ $(LIB_MACHO) created"

# Build Linux ELF support
elf: $(LIB_DIR) $(BUILD_DIR) $(LIB_ELF)
	@echo "✓ ELF support built successfully"

$(LIB_ELF): $(SRC_ELF)
	@echo "Compiling ELF C backend..."
	$(CC) $(CC_FLAGS) -o $(LIB_ELF) $(SRC_ELF)
	@echo "✓ $(LIB_ELF) created"

# Build Windows PE support
pe: $(LIB_DIR) $(BUILD_DIR) $(LIB_PE)
	@echo "✓ PE support built successfully"

$(LIB_PE): $(SRC_PE)
	@echo "Compiling PE C backend..."
	$(CC) $(CC_FLAGS) -o $(LIB_PE) $(SRC_PE)
	@echo "✓ $(LIB_PE) created"

# Cross-compile for Linux ARM64 (from macOS or other Linux)
cross-linux: $(LIB_DIR) $(BUILD_DIR)
	@echo "Cross-compiling for Linux ARM64..."
	@if command -v $(CROSS_GCC_LINUX) >/dev/null 2>&1; then \
		$(CROSS_GCC_LINUX) -shared -fPIC -O3 -Wall \
			-o $(LIB_DIR)/libkaytearm64elf.so $(SRC_ELF); \
		echo "✓ Linux ARM64 ELF library created"; \
	else \
		echo "Error: $(CROSS_GCC_LINUX) not found"; \
		echo "Install with: brew install aarch64-elf-gcc (macOS)"; \
		echo "           or: apt install gcc-aarch64-linux-gnu (Linux)"; \
		exit 1; \
	fi

# Cross-compile for Windows ARM64
cross-windows: $(LIB_DIR) $(BUILD_DIR)
	@echo "Cross-compiling for Windows ARM64..."
	@if command -v $(CROSS_GCC_WIN) >/dev/null 2>&1; then \
		$(CROSS_GCC_WIN) -shared -O3 -Wall \
			-o $(LIB_DIR)/libkaytearm64pe.dll $(SRC_PE); \
		echo "✓ Windows ARM64 PE library created"; \
	else \
		echo "Error: $(CROSS_GCC_WIN) not found"; \
		echo "Install with: brew install mingw-w64 (macOS)"; \
		echo "           or: apt install gcc-mingw-w64-aarch64 (Linux)"; \
		exit 1; \
	fi

# Build all platforms (requires cross-compilation tools)
all-platforms: macho elf pe
	@echo "✓ All platform libraries built"

# Create directories
$(LIB_DIR):
	@mkdir -p $(LIB_DIR)

$(BUILD_DIR):
	@mkdir -p $(BUILD_DIR)

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

# Test native compilation
test: native
ifeq ($(NATIVE_SUPPORT),yes)
	@echo "Running native compilation test..."
	@echo "program test; begin writeln('Hello, ARM64!'); end." > test.kayte
	@./kayte --native test.kayte -o test_native
	@if [ -f test_native ]; then \
		echo "✓ Test compilation successful"; \
		./test_native; \
		rm -f test_native test.kayte; \
	else \
		echo "✗ Test compilation failed"; \
	fi
else
	@echo "Native compilation not supported on this platform"
endif

# Install libraries to system (requires sudo)
install:
ifeq ($(OS),macos)
	@echo "Installing to /usr/local/lib..."
	@sudo cp $(LIB_MACHO) /usr/local/lib/
	@sudo install_name_tool -id /usr/local/lib/$(notdir $(LIB_MACHO)) /usr/local/lib/$(notdir $(LIB_MACHO))
else ifeq ($(OS),linux)
	@echo "Installing to /usr/local/lib..."
	@sudo cp $(LIB_ELF) /usr/local/lib/
	@sudo ldconfig
endif
	@echo "✓ Installation complete"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	@rm -rf $(BUILD_DIR)
	@rm -rf $(LIB_DIR)/*.dylib $(LIB_DIR)/*.so $(LIB_DIR)/*.dll
	@rm -f $(SRC_DIR)/*.o $(SRC_DIR)/*.ppu
	@rm -f *.o *.ppu
	@echo "✓ Clean complete"

# Deep clean (including binaries)
distclean: clean
	@echo "Deep cleaning..."
	@rm -rf $(BIN_DIR)
	@rm -f kayte$(EXE_EXT)
	@echo "✓ Deep clean complete"

# Show help
help:
	@echo "Kayte ARM64 Native Compiler - Makefile Help"
	@echo ""
	@echo "Available targets:"
	@echo "  make                 - Show info and build native support for current platform"
	@echo "  make info            - Display build environment information"
	@echo "  make native          - Build native support for current platform"
	@echo "  make macho           - Build macOS Mach-O support"
	@echo "  make elf             - Build Linux ELF support"
	@echo "  make pe              - Build Windows PE support"
	@echo "  make all-platforms   - Build all platform libraries (requires cross-tools)"
	@echo "  make cross-linux     - Cross-compile for Linux ARM64"
	@echo "  make cross-windows   - Cross-compile for Windows ARM64"
	@echo "  make test            - Test native compilation"
	@echo "  make install         - Install libraries to system (requires sudo)"
	@echo "  make clean           - Remove build artifacts"
	@echo "  make distclean       - Remove all generated files"
	@echo "  make help            - Show this help message"
	@echo ""
	@echo "Platform Support:"
	@echo "  macOS ARM64:   Mach-O format (Apple Silicon)"
	@echo "  Linux ARM64:   ELF format (AArch64)"
	@echo "  Windows ARM64: PE format"
	@echo ""
	@echo "Current Platform: $(OS)/$(ARCH)"
	@echo "Native Support:   $(NATIVE_SUPPORT)"
	@echo ""

# Debug target - show all variables
debug:
	@echo "OS:                $(OS)"
	@echo "ARCH:              $(ARCH)"
	@echo "NATIVE_SUPPORT:    $(NATIVE_SUPPORT)"
	@echo "CC:                $(CC)"
	@echo "FPC:               $(FPC)"
	@echo "LIB_MACHO:         $(LIB_MACHO)"
	@echo "LIB_ELF:           $(LIB_ELF)"
	@echo "LIB_PE:            $(LIB_PE)"
	@echo "SRC_MACHO:         $(SRC_MACHO)"
	@echo "SRC_ELF:           $(SRC_ELF)"
	@echo "SRC_PE:            $(SRC_PE)"

# Phony targets to avoid conflicts with files
.PHONY: all info native macho elf pe cross-linux cross-windows all-platforms \
        test install clean distclean help debug
