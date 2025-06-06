KAYTEC = kaytec
KAYTE_SOURCES = kayte/hello.kayte kayte/world.kayte
OBJ_FILES = build/bytecode_hello.o build/bytecode_world.o

build: $(OBJ_FILES)

build/bytecode_%.o: kayte/%.kayte
	mkdir -p build
	$(KAYTEC) $< -o build/bytecode_$*.bin
	ld -r -b binary -o $@ build/bytecode_$*.bin
	rm -f build/bytecode_$*.bin

main: build main.pas kayte_compiler.pas kayte_runtime.pas
	fpc -FEbuild -FUbuild -oKayteApp main.pas

clean:
	rm -rf build/*.o build/*.ppu build/*.a KayteApp


# Makefile for kay# Choose toolchain: lazarus or delphi
TOOLCHAIN ?= lazarus

# Project settings
PROJECT_NAME := kayte
SRC_DIR := ../source
BIN_DIR := bin


LDFLAGS += -L$(PROJECT_PATH)/lib -ln64
LDFLAGS += -macos_version_min=10.14

MAIN_FILE := projects/kayte.lpr
FPC := fpc
TARGET := $(BIN_DIR)/$(PROJECT_NAME)

# Detect platform (macOS or Linux)
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  PLATFORM := darwin
else
  PLATFORM := linux
endif

.PHONY: all clean run

all: $(TARGET)

$(TARGET): $(MAIN_FILE) $(UNIT_FILES)
	@mkdir -p $(BIN_DIR)
	$(FPC) -Fu$(SRC_DIR) -FE$(BIN_DIR) -o$(TARGET) $(MAIN_FILE)

clean:
	rm -rf bin/*.o bin/*.ppu bin/kayte

run: $(TARGET)
	./$(TARGET)

