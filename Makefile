# Makefile for kay# Choose toolchain: lazarus or delphi
TOOLCHAIN ?= lazarus

ifeq ($(TOOLCHAIN),delphi)
  include Makefile.delphi
else
  include Makefile.lazarus
endif

# Project settings
PROJECT_NAME := kayte
SRC_DIR := ../source
BIN_DIR := bin
UNIT_FILES := $(SRC_DIR)/c99.pas \
$(SRC_DIR)/sys_mac.pas \
$(SRC_DIR)/sys_ios.pas \
$(SRC_DIR)/n64.pas \
$(SRC_DIR)/cli.pas \
$(SRC_DIR)/kaytetosnes.pas \
$(SRC_DIR)/cli.pas \
$(SRC_DIR)/bytecode.pas \
$(SRC_DIR)/kayteparser.pas \
$(SRC_DIR)/sdk.pas \
$(SRC_DIR)/simplehttpserver.pas \
$(SRC_DIR)/virtualmachine.pas 


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
	rm -rf $(BIN_DIR)

run: $(TARGET)
	./$(TARGET)

