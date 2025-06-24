# Makefile for Kayte Projects
# Builds kaytec, vb6interpreter, kayteide, build_kayte, and a main app (KayteApp)
# Supports macOS (universal) and Linux (amd64, arm64) builds using FPC/Lazarus.

# --- Global Project Settings ---
PROJECT_NAME := kayte
SRC_DIR := source               # Pascal compiler/runtime source files (units)
PROJECT_DIR := projects         # Contains main .lpr project files (e.g., Kayte.lpr, vb6interpreter.lpr)
COMPONENTS_DIR := components    # Path to your components folder (relative to Makefile root)
BIN_DIR := bin                  # Where all compiled executables will go
BUILD_DIR := build              # Where intermediate bytecode objects and .o/.ppu files go

# Toolchain
FPC := fpc

# Detect platform (macOS or Linux) and set common linker flags
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  PLATFORM := darwin
  # Common linker flags for macOS. Adjust if needed.
  COMMON_LDFLAGS := -WM-macosx_version_min=10.15
  # lazbuild specific path on macOS
  LAZBUILD := /Applications/lazarus/lazbuild
  LAZARUS_APP_DIR := /Applications/lazarus # Used for lazbuild --lazarusdir
else
  PLATFORM := linux
  COMMON_LDFLAGS := # No specific common linker flags for Linux by default
  # LAZBUILD is not used directly for Linux FPC builds
endif

# --- Executables and their Main Project Files ---
# Define projects using a macro for reusability in individual build rules.
# This does NOT define the build rules directly, but makes variables available.
define DEFINE_PROJECT
$(1)_EXE_NAME := $(1)
$(1)_SOURCE_FILE := $(PROJECT_DIR)/$(1).lpr
$(1)_LPI_FILE := $(PROJECT_DIR)/$(1).lpi
$(1)_MACOS_X86_64_TARGET := $(BIN_DIR)/$(1)-macos-x86_64
$(1)_MACOS_ARM64_TARGET := $(BIN_DIR)/$(1)-macos-arm64
$(1)_LINUX_AMD64_TARGET := $(BIN_DIR)/$(1)-linux-amd64
$(1)_LINUX_ARM64_TARGET := $(BIN_DIR)/$(1)-linux-arm64
$(1)_MACOS_UNIVERSAL_TARGET := $(BIN_DIR)/$(1)-macos-universal
endef

$(eval $(call DEFINE_PROJECT,kaytec))
$(eval $(call DEFINE_PROJECT,vb6interpreter))
$(eval $(call DEFINE_PROJECT,kayteide))
$(eval $(call DEFINE_PROJECT,build_kayte))
$(eval $(call DEFINE_PROJECT,main_app)) # Assuming main_app has main_app.lpr/lpi

# --- Phony Targets ---
.PHONY: all clean _CREATE_DIRS \
        build_components build_bytecode \
        macos linux macos-x86_64 macos-arm64 linux-amd64 linux-arm64 run

# Default target: Build everything
all: macos linux

# --- Build common components (produces .ppu and .o files in BUILD_DIR) ---
# This target compiles all .pas files in COMPONENTS_DIR into .ppu/.o in BUILD_DIR.
# Other projects will then find these compiled units via -Fu$(BUILD_DIR)
build_components: _CREATE_DIRS
	@echo "Compiling components from $(COMPONENTS_DIR)..."
	$(FPC) -Fu$(SRC_DIR) -Fu$(COMPONENTS_DIR) -FE$(BUILD_DIR) -B $(COMPONENTS_DIR)/*.pas $(COMMON_LDFLAGS)
	@echo "Components compiled to $(BUILD_DIR)."

# --- Build Rules for Individual Projects (Native Architectures) ---

# macOS x86_64 builds
define MACOS_X86_64_BUILD_RULE
$(1)_MACOS_X86_64_TARGET: $($(1)_LPI_FILE) build_components | _CREATE_DIRS
	@echo "Compiling $(1) for macOS x86_64..."
	$(LAZBUILD) --lazarusdir=$(LAZARUS_APP_DIR) --os=darwin --cpu=x86_64 $($(1)_LPI_FILE)
	# lazbuild outputs to $(PROJECT_DIR)/lib/darwin-x86_64/$(1)_EXE_NAME
	mv $(PROJECT_DIR)/lib/darwin-x86_64/$(call __get_exe_name,$(1)) $@
	chmod +x $@
endef

# macOS ARM64 builds
define MACOS_ARM64_BUILD_RULE
$(1)_MACOS_ARM64_TARGET: $($(1)_LPI_FILE) build_components | _CREATE_DIRS
	@echo "Compiling $(1) for macOS arm64..."
	$(LAZBUILD) --lazarusdir=$(LAZARUS_APP_DIR) --os=darwin --cpu=aarch64 $($(1)_LPI_FILE)
	# lazbuild outputs to $(PROJECT_DIR)/lib/darwin-aarch64/$(1)_EXE_NAME
	mv $(PROJECT_DIR)/lib/darwin-aarch64/$(call __get_exe_name,$(1)) $@
	chmod +x $@
endef

# Linux AMD64 builds (assumes running on an AMD64 Linux machine)
define LINUX_AMD64_BUILD_RULE
$(1)_LINUX_AMD64_TARGET: $($(1)_SOURCE_FILE) build_components | _CREATE_DIRS
	@echo "Compiling $(1) for Linux AMD64..."
	$(FPC) $($(1)_SOURCE_FILE) -B -O3 -Tlinux -Px86_64 -FE$(BIN_DIR) -FU$(SRC_DIR) -FU$(COMPONENTS_DIR) -FU$(BUILD_DIR) -o$@ $(COMMON_LDFLAGS)
	chmod +x $@
endef

# Linux ARM64 builds (assumes running on an ARM64 Linux machine)
define LINUX_ARM64_BUILD_RULE
$(1)_LINUX_ARM64_TARGET: $($(1)_SOURCE_FILE) build_components | _CREATE_DIRS
	@echo "Compiling $(1) for Linux ARM64..."
	$(FPC) $($(1)_SOURCE_FILE) -B -O3 -Tlinux -Paarch64 -FE$(BIN_DIR) -FU$(SRC_DIR) -FU$(COMPONENTS_DIR) -FU$(BUILD_DIR) -o$@ $(COMMON_LDFLAGS)
	chmod +x $@
endef

# Helper to get the base executable name (e.g., "kaytec" from "kaytec_EXE_NAME")
# lazbuild's mv needs this because it outputs the raw executable name
__get_exe_name = $(value $(1)_EXE_NAME)


# Apply the build rules for each project defined above
$(eval $(call MACOS_X86_64_BUILD_RULE,kaytec))
$(eval $(call MACOS_X86_64_BUILD_RULE,vb6interpreter))
$(eval $(call MACOS_X86_64_BUILD_RULE,kayteide))
$(eval $(call MACOS_X86_64_BUILD_RULE,build_kayte))
$(eval $(call MACOS_X86_64_BUILD_RULE,main_app))

$(eval $(call MACOS_ARM64_BUILD_RULE,kaytec))
$(eval $(call MACOS_ARM64_BUILD_RULE,vb6interpreter))
$(eval $(call MACOS_ARM64_BUILD_RULE,kayteide))
$(eval $(call MACOS_ARM64_BUILD_RULE,build_kayte))
$(eval $(call MACOS_ARM64_BUILD_RULE,main_app))

$(eval $(call LINUX_AMD64_BUILD_RULE,kaytec))
$(eval $(call LINUX_AMD64_BUILD_RULE,vb6interpreter))
$(eval $(call LINUX_AMD64_BUILD_RULE,kayteide))
$(eval $(call LINUX_AMD64_BUILD_RULE,build_kayte))
$(eval $(call LINUX_AMD64_BUILD_RULE,main_app))

$(eval $(call LINUX_ARM64_BUILD_RULE,kaytec))
$(eval $(call LINUX_ARM64_BUILD_RULE,vb6interpreter))
$(eval $(call LINUX_ARM64_BUILD_RULE,kayteide))
$(eval $(call LINUX_ARM64_BUILD_RULE,build_kayte))
$(eval $(call LINUX_ARM64_BUILD_RULE,main_app))


# --- Build Targets by OS/Architecture ---

macos: macos-x86_64 macos-arm64
	@echo "All macOS native builds completed. Proceeding to universal linking..."
	# Kayte Compiler Universal
	lipo -create -output $(kaytec_MACOS_UNIVERSAL_TARGET) $(kaytec_MACOS_X86_64_TARGET) $(kaytec_MACOS_ARM64_TARGET)
	chmod +x $(kaytec_MACOS_UNIVERSAL_TARGET)
	
	# VB6 Interpreter Universal
	lipo -create -output $(vb6interpreter_MACOS_UNIVERSAL_TARGET) $(vb6interpreter_MACOS_X86_64_TARGET) $(vb6interpreter_MACOS_ARM64_TARGET)
	chmod +x $(vb6interpreter_MACOS_UNIVERSAL_TARGET)

	# Kayte IDE Universal
	lipo -create -output $(kayteide_MACOS_UNIVERSAL_TARGET) $(kayteide_MACOS_X86_64_TARGET) $(kayteide_MACOS_ARM64_TARGET)
	chmod +x $(kayteide_MACOS_UNIVERSAL_TARGET)

	# build_kayte Universal
	lipo -create -output $(build_kayte_MACOS_UNIVERSAL_TARGET) $(build_kayte_MACOS_X86_64_TARGET) $(build_kayte_MACOS_ARM64_TARGET)
	chmod +x $(build_kayte_MACOS_UNIVERSAL_TARGET)

	# main_app Universal
	lipo -create -output $(main_app_MACOS_UNIVERSAL_TARGET) $(main_app_MACOS_X86_64_TARGET) $(main_app_MACOS_ARM64_TARGET)
	chmod +x $(main_app_MACOS_UNIVERSAL_TARGET)

	@echo "macOS universal binaries created."

macos-x86_64: $(kaytec_MACOS_X86_64_TARGET) $(vb6interpreter_MACOS_X86_64_TARGET) $(kayteide_MACOS_X86_64_TARGET) $(build_kayte_MACOS_X86_64_TARGET) $(main_app_MACOS_X86_64_TARGET)
	@echo "All macOS x86_64 binaries built."

macos-arm64: $(kaytec_MACOS_ARM64_TARGET) $(vb6interpreter_MACOS_ARM64_TARGET) $(kayteide_MACOS_ARM64_TARGET) $(build_kayte_MACOS_ARM64_TARGET) $(main_app_MACOS_ARM64_TARGET)
	@echo "All macOS arm64 binaries built."

linux: linux-amd64 linux-arm64

linux-amd64: $(kaytec_LINUX_AMD64_TARGET) $(vb6interpreter_LINUX_AMD64_TARGET) $(kayteide_LINUX_AMD64_TARGET) $(build_kayte_LINUX_AMD64_TARGET) $(main_app_LINUX_AMD64_TARGET)
	@echo "All Linux AMD64 binaries built."

linux-arm64: $(kaytec_LINUX_ARM64_TARGET) $(vb6interpreter_LINUX_ARM64_TARGET) $(kayteide_LINUX_ARM64_TARGET) $(build_kayte_LINUX_ARM64_TARGET) $(main_app_LINUX_ARM64_TARGET)
	@echo "All Linux ARM64 binaries built."

# --- Kayte Bytecode Generation ---
KAYTE_SOURCES = kayte/hello.kayte kayte/world.kayte # Example Kayte source files
OBJ_FILES = $(patsubst kayte/%.kayte, $(BUILD_DIR)/bytecode_%.o, $(KAYTE_SOURCES))

# This rule depends on the macOS x86_64 kaytec to generate bytecode.
# If you need to generate bytecode on a Linux agent, you would need a separate rule
# that uses a Linux-built kaytec.
$(BUILD_DIR)/bytecode_%.o: kayte/%.kayte $(kaytec_MACOS_X86_64_TARGET) | _CREATE_DIRS
	@echo "Generating bytecode for $< and embedding into object file..."
	$(kaytec_MACOS_X86_64_TARGET) $< -o $(BUILD_DIR)/bytecode_$*.bin
	ld -r -b binary -o $@ $(BUILD_DIR)/bytecode_$*.bin
	rm -f $(BUILD_DIR)/bytecode_$*.bin
	@echo "Built bytecode object: $@"


# --- Directory Creation Helper Target ---
# This target just ensures the directories exist. It is phony because it doesn't
# represent actual files to be built, but an action to create directories.
_CREATE_DIRS:
	@echo "Ensuring build directories exist ($(BIN_DIR) and $(BUILD_DIR))..."
	mkdir -p $(BIN_DIR) $(BUILD_DIR)


# --- Cleanup ---
clean:
	@echo "Cleaning up build artifacts and executables..."
	rm -rf $(BIN_DIR)
	rm -rf $(BUILD_DIR)
	rm -rf $(PROJECT_DIR)/lib # Remove lazbuild temporary directories
	@echo "Cleanup complete."

# --- Run the Main Application ---
run: $(main_app_MACOS_UNIVERSAL_TARGET) # Default to run the macOS universal app
	@echo "Running $(main_app_MACOS_UNIVERSAL_TARGET)..."
	./$(main_app_MACOS_UNIVERSAL_TARGET)

# If you want to run specific Linux builds, you'd define separate 'run-linux-amd64' etc.
# run-linux-amd64: $(main_app_LINUX_AMD64_TARGET)
#	./$(main_app_LINUX_AMD64_TARGET)
