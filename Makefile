# Makefile for Kayte Projects
# Builds kaytec, vb6interpreter, kayteide, build_kayte, and a main app (KayteApp)
# Supports macOS (universal) and Linux (amd64, arm64) builds using FPC/Lazarus.

# --- Global Project Settings ---
PROJECT_NAME := kayte
SRC_DIR := source                # Pascal compiler/runtime source files (units)
PROJECT_DIR := projects          # Contains main .lpr project files (e.g., Kayte.lpr, vb6interpreter.lpr)
COMPONENTS_DIR := components     # Path to your components folder (relative to Makefile root)
BIN_DIR := bin                   # Where all compiled executables will go
BUILD_DIR := build               # Where intermediate bytecode objects and .o/.ppu files go

# Toolchain
FPC := fpc
LAZBUILD := /Applications/lazarus/lazbuild # Default for macOS, adjust if needed
LAZARUS_APP_DIR := /Applications/lazarus # Used for lazbuild --lazarusdir

# Detect platform (macOS or Linux) and set common linker flags
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
  PLATFORM := linux
  COMMON_LDFLAGS := # No specific common linker flags for Linux by default
else ifeq ($(UNAME_S),Darwin)
  PLATFORM := darwin
  COMMON_LDFLAGS := -WM-macosx_version_min=11.0 # Common linker flags for macOS
else
  $(error Unsupported OS: $(UNAME_S). Please add specific rules for it.)
endif

# --- Project Definitions ---
# List of all project base names (without .lpr/.lpi or paths)
PROJECT_BASE_NAMES = kaytec vb6interpreter kayteide build_kayte main_app

# --- Helper Functions ---
# Function to get the full .lpr path for a given project base name
define get_lpr_path
$(PROJECT_DIR)/$(1).lpr
endef

# Function to get the full .lpi path for a given project base name
define get_lpi_path
$(PROJECT_DIR)/$(1).lpi
endef

# Function to get the full executable path for a given project base name and OS/arch
define get_exe_path
$(BIN_DIR)/$(1)-$(2)-$(3)
endef

# --- All Source and Target Lists ---
# All .pas unit source files from SRC_DIR and COMPONENTS_DIR
ALL_UNIT_SOURCES = $(wildcard $(SRC_DIR)/*.pas) $(wildcard $(COMPONENTS_DIR)/*.pas)

# All .ppu targets (compiled units in BUILD_DIR)
ALL_PPU_TARGETS = $(patsubst %.pas,$(BUILD_DIR)/%.ppu,$(notdir $(ALL_UNIT_SOURCES)))

# All executable targets for each platform/architecture
ALL_MACOS_X86_64_TARGETS = $(foreach p,$(PROJECT_BASE_NAMES),$(call get_exe_path,$(p),macos,x86_64))
ALL_MACOS_ARM64_TARGETS = $(foreach p,$(PROJECT_BASE_NAMES),$(call get_exe_path,$(p),macos,arm64))
ALL_LINUX_AMD64_TARGETS = $(foreach p,$(PROJECT_BASE_NAMES),$(call get_exe_path,$(p),linux,amd64))
ALL_LINUX_ARM64_TARGETS = $(foreach p,$(PROJECT_BASE_NAMES),$(call get_exe_path,$(p),linux,arm64))

# All universal macOS targets
ALL_MACOS_UNIVERSAL_TARGETS = $(foreach p,$(PROJECT_BASE_NAMES),$(BIN_DIR)/$(p)-macos-universal)


# --- Phony Targets ---
.PHONY: all clean _CREATE_DIRS \
        build_components build_bytecode \
        macos linux macos-x86_64 macos-arm64 linux-amd64 linux-arm64 \
        universal run run-x86_64-on-arm64

# Default target: Build everything
all: macos linux

# --- Directory Creation Targets ---
# These are actual directory targets. Make will create them if they don't exist.
$(BIN_DIR) $(BUILD_DIR):
	@echo "Creating directory: $@"
	mkdir -p $@

# --- VPATH for Make to find source files ---
# This tells make to look in these directories for prerequisites like %.pas, %.lpr, %.lpi
vpath %.pas $(SRC_DIR):$(COMPONENTS_DIR)
vpath %.lpr $(PROJECT_DIR)
vpath %.lpi $(PROJECT_DIR)


# --- Build common components (produces .ppu and .o files in BUILD_DIR) ---
# This target compiles all .pas files in ALL_UNIT_SOURCES into .ppu/.o in BUILD_DIR.
build_components: $(ALL_PPU_TARGETS)
	@echo "All common components compiled to $(BUILD_DIR)."

# Generic rule for compiling any .pas unit into its .ppu/.o in BUILD_DIR
# The prerequisite `%.pas` will be found by `vpath`
$(BUILD_DIR)/%.ppu: %.pas | $(BUILD_DIR)
	@echo "Compiling unit $<..."
	$(FPC) -MObjFPC -O2 -g -gl -vewnhi -FE$(BUILD_DIR) -FU$(BUILD_DIR) -Fu$(SRC_DIR) -Fu$(COMPONENTS_DIR) $<


# --- Generic Build Rules for Executables ---
# These rules use pattern matching to build any executable based on its .lpr or .lpi file.

# Common FPC compilation flags for executables
FPC_EXE_FLAGS = -MObjFPC -O3 -g -gl -vewnhi -FU$(BUILD_DIR) -Fu$(SRC_DIR) -Fu$(COMPONENTS_DIR) $(COMMON_LDFLAGS)

# macOS builds (using lazbuild for .lpi projects)
$(BIN_DIR)/%-macos-x86_64: %.lpi build_components | $(BIN_DIR)
	@echo "Compiling $* for macOS x86_64 using lazbuild..."
	$(LAZBUILD) --lazarusdir=$(LAZARUS_APP_DIR) --os=darwin --cpu=x86_64 $<
	# lazbuild outputs to $(PROJECT_DIR)/lib/darwin-x86_64/$(basename $(notdir $<))
	mv $(PROJECT_DIR)/lib/darwin-x86_64/$(basename $(notdir $<)) $@
	chmod +x $@

$(BIN_DIR)/%-macos-arm64: %.lpi build_components | $(BIN_DIR)
	@echo "Compiling $* for macOS arm64 using lazbuild..."
	$(LAZBUILD) --lazarusdir=$(LAZARUS_APP_DIR) --os=darwin --cpu=aarch64 $<
	mv $(PROJECT_DIR)/lib/darwin-aarch64/$(basename $(notdir $<)) $@
	chmod +x $@

# Linux builds (using fpc directly for .lpr projects)
$(BIN_DIR)/%-linux-amd64: %.lpr build_components | $(BIN_DIR)
	@echo "Compiling $* for Linux AMD64 using fpc..."
	$(FPC) $< -Tlinux -Px86_64 -FE$(BIN_DIR) -o$@ $(FPC_EXE_FLAGS)
	chmod +x $@

$(BIN_DIR)/%-linux-arm64: %.lpr build_components | $(BIN_DIR)
	@echo "Compiling $* for Linux ARM64 using fpc..."
	$(FPC) $< -Tlinux -Paarch64 -FE$(BIN_DIR) -o$@ $(FPC_EXE_FLAGS)
	chmod +x $@


# --- Aggregate Build Targets by OS/Architecture ---

macos: $(ALL_MACOS_X86_64_TARGETS) $(ALL_MACOS_ARM64_TARGETS) universal
	@echo "All macOS builds completed."

linux: $(ALL_LINUX_AMD64_TARGETS) $(ALL_LINUX_ARM64_TARGETS)
	@echo "All Linux builds completed."

# Universal macOS binaries
universal: $(ALL_MACOS_UNIVERSAL_TARGETS)
	@echo "macOS universal binaries created."

$(BIN_DIR)/%-macos-universal: $(BIN_DIR)/%-macos-x86_64 $(BIN_DIR)/%-macos-arm64
	@echo "Creating universal binary for $*..."
	lipo -create -output $@ $< $(word 2,$^) # $< is first prereq, $(word 2,$^) is second
	chmod +x $@


# --- Kayte Bytecode Generation ---
KAYTE_SOURCES = kayte/hello.kayte kayte/world.kayte # Example Kayte source files
OBJ_FILES = $(patsubst kayte/%.kayte, $(BUILD_DIR)/bytecode_%.o, $(KAYTE_SOURCES))

# This rule depends on the macOS x86_64 kaytec to generate bytecode.
$(BUILD_DIR)/bytecode_%.o: kayte/%.kayte $(call get_exe_path,kaytec,macos,x86_64) | $(BUILD_DIR)
	@echo "Generating bytecode for $< and embedding into object file..."
	$(call get_exe_path,kaytec,macos,x86_64) $< -o $(BUILD_DIR)/bytecode_$*.bin
	ld -r -b binary -o $@ $(BUILD_DIR)/bytecode_$*.bin
	rm -f $(BUILD_DIR)/bytecode_$*.bin
	@echo "Built bytecode object: $@"


# --- Cleanup ---
clean:
	@echo "Cleaning up build artifacts and executables..."
	rm -rf $(BIN_DIR)
	rm -rf $(BUILD_DIR)
	@echo "Cleanup complete."

# --- Run the Main Application ---
run: $(call get_exe_path,main_app,macos,universal) # Default to run the macOS universal app
	@echo "Running $(call get_exe_path,main_app,macos,universal)..."
	./$(call get_exe_path,main_app,macos,universal)

# --- Run an x86_64 macOS binary on an Apple Silicon (ARM64) Mac via Rosetta 2 ---
# This target is only useful when run on an ARM64 macOS machine.
run-x86_64-on-arm64: $(call get_exe_path,main_app,macos,x86_64)
	@echo "Attempting to run x86_64 macOS app via Rosetta 2..."
	@echo "This command will only work on an Apple Silicon Mac with Rosetta 2 installed."
	arch -x86_64 ./$(call get_exe_path,main_app,macos,x86_64)

# Example for running specific Linux builds
run-linux-amd64: $(call get_exe_path,main_app,linux,amd64)
	@echo "Running $(call get_exe_path,main_app,linux,amd64)..."
	./$(call get_exe_path,main_app,linux,amd64)

run-linux-arm64: $(call get_exe_path,main_app,linux,arm64)
	@echo "Running $(call get_exe_path,main_app,linux,arm64)..."
	./$(call get_exe_path,main_app,linux,arm64)
