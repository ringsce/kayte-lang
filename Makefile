# Main Makefile for Kings Project

APP_NAME := kings
SRC_DIR := src
BUILD_DIR := build
FPC := fpc

.PHONY: all clean

all: $(BUILD_DIR)/$(APP_NAME)

$(BUILD_DIR)/$(APP_NAME): $(SRC_DIR)/*.pas Makefile.fpc
	mkdir -p $(BUILD_DIR)
	$(MAKE) -f Makefile.fpc

clean:
	rm -rf $(BUILD_DIR)

