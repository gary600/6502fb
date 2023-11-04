SIM ?= iverilog
SIM_OPTS += -g2005-sv -Ttyp

SV_FILES += $(wildcard *.sv)

.PHONY: all run
all: sim
run: vga.vcd

sim: $(SV_FILES)
	$(SIM) $(SIM_OPTS) -o sim $(SV_FILES)

.PRECIOUS: vga.vcd
vga.vcd: sim
	./sim +DUMP=vga.vcd

clean:
	-rm vga.vcd sim