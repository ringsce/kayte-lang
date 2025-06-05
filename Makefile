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

