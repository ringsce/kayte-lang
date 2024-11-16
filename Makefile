# Makefile for building with LLVM and Clang on macOS

# Project configuration
PROJECT_NAME := myproject
SOURCE_FILES := $(wildcard *.pas)
OUTPUT_FILE := $(PROJECT_NAME)

# Compiler and tools
FPC := fpc
CLANG := clang
LLVM_LINK := llvm-link
LD := ld
AR := ar
RM := rm -f
OS := darwin
CPU := x86_64
MACOS_MIN_VERSION := 11

# Compiler and linker flags
FPC_FLAGS := -B -Mobjfpc -S2 -O2 -Xs -XX -CX -Xr
CLANG_FLAGS := -target $(CPU)-apple-macos$(MACOS_MIN_VERSION)
LDFLAGS := -framework Cocoa -framework CoreFoundation -macosx_version_min $(MACOS_MIN_VERSION)

# Output directories
BUILD_DIR := build
BIN_DIR := bin
OBJ_DIR := $(BUILD_DIR)/obj
UNIT_DIR := $(BUILD_DIR)/units

# Ensure directories exist
$(shell mkdir -p $(BUILD_DIR) $(BIN_DIR) $(OBJ_DIR) $(UNIT_DIR))

# Default target
.PHONY: all
all: $(BIN_DIR)/$(OUTPUT_FILE)

# Build target
$(BIN_DIR)/$(OUTPUT_FILE): $(SOURCE_FILES)
	@echo "Building $(PROJECT_NAME) with FPC..."
	$(FPC) $(FPC_FLAGS) -o$(BIN_DIR)/$(OUTPUT_FILE) -FU$(UNIT_DIR) -FE$(OBJ_DIR) $^

# Debug build
.PHONY: debug
debug: FPC_FLAGS += -gl -dDEBUG
debug: clean all

# Release build
.PHONY: release
release: FPC_FLAGS += -O3 -Xs -dRELEASE
release: clean all

# LLVM Backend build (experimental)
.PHONY: llvm
llvm:
	@echo "Building with LLVM and Clang..."
	$(CLANG) $(CLANG_FLAGS) $(LDFLAGS) -o $(BIN_DIR)/$(OUTPUT_FILE) $(SOURCE_FILES)

# Clean target
.PHONY: clean
clean:
	@echo "Cleaning up..."
	$(RM) $(BIN_DIR)/$(OUTPUT_FILE) $(OBJ_DIR)/* $(UNIT_DIR)/*

# Full clean including build directory
.PHONY: distclean
distclean: clean
	@rm -rf $(BUILD_DIR) $(BIN_DIR)

# Info target
.PHONY: info
info:
	@echo "Project: $(PROJECT_NAME)"
	@echo "Source Files: $(SOURCE_FILES)"
	@echo "Output File: $(BIN_DIR)/$(OUTPUT_FILE)"
	@echo "Compiler: $(FPC)"
	@echo "Clang: $(CLANG)"
	@echo "LLVM Linker: $(LLVM_LINK)"

