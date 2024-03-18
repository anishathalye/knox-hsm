.PHONY: all
all: fw/counter.mem

.PHONY: clean
clean:
	rm -rf $(CLEAN_FILES)

HEADERFILES := $(shell find fw -name '*.h')

include ../../common/impl/Makefrag

ROM_DEPTH := 1024

include fw/Makefrag

SOC_V := $(shell find hw/soc -name '*.v')
BOARD_V := hw/counter.v $(SOC_V)
FPGA_V := hw/fpga.v hw/sync.v $(BOARD_V)

CLEAN_FILES += fpga.json fpga.asc fpga.rpt fpga.bin fpga.yosys.log fpga.pnr.log
fpga.json: $(FPGA_V) fw/counter.mem
	yosys -ql fpga.yosys.log -p 'read_verilog -defer $(FPGA_V)' -p 'synth_ice40 -top fpga -json $@'

.PHONY: prog
prog: fpga.bin
	iceprog $<
