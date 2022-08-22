.PHONY: all
all: fifo.rkt

.PHONY: clean
clean:
	rm -rf fifo.smt2 fifo.rkt

fifo.smt2: ../impl/fifo.v
	yosys \
		-p 'read_verilog -defer $^' \
		-p 'prep -flatten -top fifo -nordff' \
		-p 'write_smt2 -stdt fifo.smt2'

%.rkt: %.smt2
	echo '#lang yosys' > $@
	cat $< >> $@
