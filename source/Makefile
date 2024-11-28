# Makefile for Delphi 12 Community Edition and Lazarus 3.x on macOS

# Project configuration
PROJECT_NAME := MyProject
SOURCE_FILES := $(wildcard *.pas)
OUTPUT_FILE := $(PROJECT_NAME)

# Environment selection
# Set to DELPHI for Delphi 12 or LAZARUS for Lazarus 3.x
ENVIRONMENT ?= LAZARUS

# Compiler and tools
FPC := fpc
DELPHIC := dcc
RM := rm -f

# Directories
BUILD_DIR := build
BIN_DIR := bin
UNIT_DIR := $(BUILD_DIR)/units

# macOS configuration
MACOS_MIN_VERSION := 10.15
FRAMEWORKS := -framework Cocoa -framework CoreFoundation

# Lazarus-specific settings
LAZARUS_FLAGS := -Mobjfpc -Sc -O2 -Xs -XX -CX -Xr -FU$(UNIT_DIR) -FE$(BUILD_DIR)

# Delphi-specific settings
DELPHI_FLAGS := -CC -NSSystem.SysUtils,System.Classes -U$(BUILD_DIR) -Q -$O- -$D- -$L+

# Ensure build directories exist
$(shell mkdir -p $(BUILD_DIR) $(BIN_DIR) $(UNIT_DIR))

# Default target
.PHONY: all
all: $(BIN_DIR)/$(OUTPUT_FILE)

# Build target
$(BIN_DIR)/$(OUTPUT_FILE): $(SOURCE_FILES)
ifeq ($(ENVIRONMENT),LAZARUS)
	@echo "Building with Lazarus (Free Pascal)..."
	$(FPC) $(LAZARUS_FLAGS) -o$(BIN_DIR)/$(OUTPUT_FILE) $^
else ifeq ($(ENVIRONMENT),DELPHI)
	@echo "Building with Delphi 12..."
	$(DELPHIC) $(DELPHI_FLAGS) -o$(BIN_DIR)/$(OUTPUT_FILE) $^
else
	$(error Unknown environment: $(ENVIRONMENT). Please set ENVIRONMENT to LAZARUS or DELPHI.)
endif

# Debug build
.PHONY: debug
debug: clean
ifeq ($(ENVIRONMENT),LAZARUS)
	$(MAKE) FPC_FLAGS="-gl -dDEBUG" all
else ifeq ($(ENVIRONMENT),DELPHI)
	$(MAKE) DELPHI_FLAGS="-CC -DDEBUG" all
endif

# Release build
.PHONY: release
release: clean
ifeq ($(ENVIRONMENT),LAZARUS)
	$(MAKE) FPC_FLAGS="-O3 -dRELEASE" all
else ifeq ($(ENVIRONMENT),DELPHI)
	$(MAKE) DELPHI_FLAGS="-CC -DRELEASE" all
endif

# Clean target
.PHONY: clean
clean:
	@echo "Cleaning up..."
	$(RM) $(BIN_DIR)/$(OUTPUT_FILE) $(BUILD_DIR)/*

# Info target
.PHONY: info
info:
	@echo "Project: $(PROJECT_NAME)"
	@echo "Environment: $(ENVIRONMENT)"
	@echo "Source Files: $(SOURCE_FILES)"
	@echo "Output File: $(BIN_DIR)/$(OUTPUT_FILE)"
