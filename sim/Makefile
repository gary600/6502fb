ICARUS ?= iverilog
ICARUS_OPTS += -g2005-sv

SV_FILES = $(wildcard *.sv)

.PHONY: all
all: sim

sim: $(SV_FILES)
	$(ICARUS) $(ICARUS_OPTS) -o sim $(SV_FILES)

vga.vcd: sim
	./sim