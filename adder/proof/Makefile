.PHONY: all
all: adder.rkt

.PHONY: clean
clean:
	rm -rf adder.smt2 adder.rkt

adder.smt2: ../impl/adder.v
	yosys \
		-p 'read_verilog -defer $^' \
		-p 'prep -flatten -top adder -nordff' \
		-p 'write_smt2 -stdt adder.smt2'

%.rkt: %.smt2
	echo '#lang yosys' > $@
	cat $< >> $@
