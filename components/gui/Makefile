# Makefile to compile Kayte Lang components

FPC = fpc
SRC = $(wildcard *.pas)
OUTDIR = build
UNITS = -Fu$(OUTDIR)
PPUFLAGS = -MObjFPC -Scgi -O2 -g -gl -l -vewnhi -Fu. -FE$(OUTDIR) -FU$(OUTDIR)

.PHONY: all clean

all: $(OUTDIR) $(SRC:.pas=.ppu)

$(OUTDIR):
	mkdir -p $(OUTDIR)

%.ppu: %.pas
	$(FPC) $(PPUFLAGS) $<

clean:
	rm -rf $(OUTDIR)
