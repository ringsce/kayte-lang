# --- Global Project Settings ---
PROJECT_NAME := kayte
SRC_DIR := source               # Pascal compiler/runtime source files (units)
PROJECT_DIR := projects         # Contains main .lpr project files
COMPONENTS_DIR := ../components # NEW: Path to your components folder
BIN_DIR := bin                  # Where all compiled executables will go
BUILD_DIR := build              # Where intermediate bytecode objects and .o/.ppu files go

# Toolchain
FPC := fpc

# Detect platform (macOS or Linux)
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  PLATFORM := darwin
  # Common linker flags for macOS. Adjust if needed.
  COMMON_LDFLAGS := -macos_version_min=10.15
else
  PLATFORM := linux
  COMMON_LDFLAGS := # No specific common linker flags for Linux by default
endif

# --- Executables and their Main Project Files ---
KAYTEC_EXE_NAME := kaytec # Explicitly name the compiler executable 'kaytec'
KAYTEC_SOURCE_FILE := $(PROJECT_DIR)/$(PROJECT_NAME).lpr
KAYTEC_TARGET := $(BIN_DIR)/$(KAYTEC_EXE_NAME)

VB6_INTERPRETER_EXE_NAME := vb6interpreter
VB6_INTERPRETER_SOURCE_FILE := $(PROJECT_DIR)/vb6interpreter.lpr
VB6_INTERPRETER_TARGET := $(BIN_DIR)/$(VB6_INTERPRETER_EXE_NAME)

KAYTE_IDE_EXE_NAME := kayteide
KAYTE_IDE_SOURCE_FILE := $(PROJECT_DIR)/kayteide.lpr
KAYTE_IDE_TARGET := $(BIN_DIR)/$(KAYTE_IDE_EXE_NAME)

BUILD_KAYTE_EXE_NAME := build_kayte
BUILD_KAYTE_SOURCE_FILE := $(PROJECT_DIR)/build_kayte.lpr
BUILD_KAYTE_TARGET := $(BIN_DIR)/$(BUILD_KAYTE_EXE_NAME)

# --- Bytecode Generation (using KAYTEC_TARGET) ---
KAYTE_SOURCES = kayte/hello.kayte kayte/world.kayte
OBJ_FILES = $(patsubst kayte/%.kayte, $(BUILD_DIR)/bytecode_%.o, $(KAYTE_SOURCES))

# --- Main Application (linking bytecode) ---
MAIN_APP_SOURCE_FILE := main.pas
MAIN_APP_TARGET := KayteApp # This creates KayteApp in the root directory

# --- Common FPC Flags for all projects ---
# -Fu: Unit search path (source dirs and output dir for compiled units)
# -FE: Executable output directory
# -FE$(BUILD_DIR) for units, -FE$(BIN_DIR) for executables
COMMON_FPC_FLAGS := -Fu$(SRC_DIR) -Fu$(COMPONENTS_DIR) -FE$(BUILD_DIR)

.PHONY: all build_components build_kaytec build_vb6_interpreter build_kayte_ide build_kayte \
        build_bytecode main_app clean run

# Define the directories as a list. These will be created if they don't exist.
DIRS := $(BIN_DIR) $(BUILD_DIR)

# Default target: Build everything
all: build_components build_kaytec build_vb6_interpreter build_kayte_ide build_kayte build_bytecode main_app

# --- Build common components (produces .ppu and .o files in BUILD_DIR) ---
# This target compiles all .pas files in COMPONENTS_DIR into .ppu/.o in BUILD_DIR.
# Other projects will then find these compiled units via -Fu$(BUILD_DIR)
build_components: $(DIRS)
	@echo "Compiling components from $(COMPONENTS_DIR)..."
	$(FPC) $(COMMON_FPC_FLAGS) -I$(COMPONENTS_DIR) $(COMPONENTS_DIR)/*.pas # Compiles all .pas in components
	@echo "Components compiled to $(BUILD_DIR)."


# --- Build the Kayte Compiler (kaytec) ---
build_kaytec: $(KAYTEC_TARGET)
$(KAYTEC_TARGET): $(KAYTEC_SOURCE_FILE) build_components | $(DIRS) # Depends on build_components
	$(FPC) $(COMMON_FPC_FLAGS) -FE$(BIN_DIR) -o$(KAYTEC_TARGET) $(KAYTEC_SOURCE_FILE) $(COMMON_LDFLAGS)
	@echo "Kayte compiler (kaytec) built at: $(KAYTEC_TARGET)"

# --- Build the VB6 Interpreter ---
build_vb6_interpreter: $(VB6_INTERPRETER_TARGET)
$(VB6_INTERPRETER_TARGET): $(VB6_INTERPRETER_SOURCE_FILE) build_components | $(DIRS) # Depends on build_components
	$(FPC) $(COMMON_FPC_FLAGS) -FE$(BIN_DIR) -o$(VB6_INTERPRETER_TARGET) $(VB6_INTERPRETER_SOURCE_FILE) $(COMMON_LDFLAGS)
	@echo "VB6 Interpreter built at: $(VB6_INTERPRETER_TARGET)"

# --- Build the Kayte IDE ---
build_kayte_ide: $(KAYTE_IDE_TARGET)
$(KAYTE_IDE_TARGET): $(KAYTE_IDE_SOURCE_FILE) build_components | $(DIRS) # Depends on build_components
	$(FPC) $(COMMON_FPC_FLAGS) -FE$(BIN_DIR) -o$(KAYTE_IDE_TARGET) $(KAYTE_IDE_SOURCE_FILE) $(COMMON_LDFLAGS)
	@echo "Kayte IDE built at: $(KAYTE_IDE_TARGET)"

# --- Build the new build_kayte executable ---
build_kayte: $(BUILD_KAYTE_TARGET)
$(BUILD_KAYTE_TARGET): $(BUILD_KAYTE_SOURCE_FILE) build_components | $(DIRS) # Depends on build_components
	$(FPC) $(COMMON_FPC_FLAGS) -FE$(BIN_DIR) -o$(BUILD_KAYTE_TARGET) $(BUILD_KAYTE_SOURCE_FILE) $(COMMON_LDFLAGS)
	@echo "build_kayte executable built at: $(BUILD_KAYTE_TARGET)"

# --- Build the Kayte bytecode object files using the 'kaytec' compiler ---
build_bytecode: $(OBJ_FILES)
$(BUILD_DIR)/bytecode_%.o: kayte/%.kayte $(KAYTEC_TARGET) | $(DIRS)
	# Use the full path to the built kaytec executable
	$(KAYTEC_TARGET) $< -o $(BUILD_DIR)/bytecode_$*.bin
	ld -r -b binary -o $@ $(BUILD_DIR)/bytecode_$*.bin
	rm -f $(BUILD_DIR)/bytecode_$*.bin
	@echo "Built bytecode object: $@"

# --- Build the main application that uses the bytecode ---
# This target depends on the bytecode objects
main_app: $(MAIN_APP_TARGET)
$(MAIN_APP_TARGET): $(MAIN_APP_SOURCE_FILE) kayte_compiler.pas kayte_runtime.pas $(OBJ_FILES) build_components | $(DIRS) # Depends on build_components
	$(FPC) $(COMMON_FPC_FLAGS) -o$(MAIN_APP_TARGET) $(MAIN_APP_SOURCE_FILE) $(COMMON_LDFLAGS)
	@echo "Main application $(MAIN_APP_TARGET) built."

# --- Directory Creation Rules ---
# This special rule creates directories if they don't exist.
# The '|' in other rules makes these order-only prerequisites.
$(DIRS):
	mkdir -p $@

# --- Cleanup ---
clean:
	@echo "Cleaning up build artifacts and executables..."
	rm -rf $(BIN_DIR)
	rm -rf $(BUILD_DIR)
	rm -f $(MAIN_APP_TARGET)
	@echo "Cleanup complete."

# --- Run the Main Application ---
run: main_app
	./$(MAIN_APP_TARGET)
