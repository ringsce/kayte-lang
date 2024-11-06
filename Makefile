# Makefile for Delphi 11 CE (Windows/macOS) and Lazarus (Linux/Windows/macOS)
PROJECT_NAME = kings
SRC_FILES = $(PROJECT_NAME).pas
DELPHI_OUT = bin/$(PROJECT_NAME).exe
FPC_OUT = bin/$(PROJECT_NAME)

# Macros for platforms
ifeq ($(OS),Windows_NT)
    DELPHI_COMPILER = dcc32
    DELPHI_FLAGS = -U..\units\ -B -Q
    FPC_COMPILER = fpc
else ifeq ($(shell uname), Darwin)
    DELPHI_COMPILER = dccosx
    DELPHI_FLAGS = -U../units/ -B -Q
    FPC_COMPILER = fpc
else
    FPC_COMPILER = fpc
endif

.PHONY: all clean

all: $(FPC_OUT)

$(FPC_OUT):
    @echo "Building for platform $(OS)..."
    @if [ -f $(DELPHI_COMPILER) ]; then \
        $(DELPHI_COMPILER) $(SRC_FILES) -Ebin $(DELPHI_FLAGS); \
    else \
        $(FPC_COMPILER) $(SRC_FILES) -obin/$(PROJECT_NAME); \
    fi

clean:
    rm -f bin/$(PROJECT_NAME)*

