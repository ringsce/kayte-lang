# Makefile
.PHONY: build clean

# Define output paths and compiler options
SRC = src/kayte-core.pas
DIST = dist
OUTPUT = $(DIST)/kayte-core

build: $(OUTPUT).node

$(OUTPUT).node: $(SRC)
	@mkdir -p $(DIST)
	fpc -MObjFPC -Sh $(SRC) -o$(OUTPUT).node -Opnodejs

clean:
	rm -rf $(DIST)/*

