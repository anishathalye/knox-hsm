.PHONY: all
all: counter.rkt

.PHONY: clean
clean:
	rm -rf counter.smt2 counter.rkt

counter.smt2: ../impl/counter.v
	yosys \
		-p 'read_verilog -defer $^' \
		-p 'prep -flatten -top counter -nordff' \
		-p 'write_smt2 -stdt counter.smt2'

%.rkt: %.smt2
	echo '#lang yosys' > $@
	cat $< >> $@
