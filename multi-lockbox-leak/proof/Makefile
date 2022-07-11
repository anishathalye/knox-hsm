.PHONY: all
all: lockbox.rkt

.PHONY: clean
clean:
	rm -rf lockbox.smt2 lockbox.rkt

lockbox.smt2: ../impl/lockbox.v
	yosys \
		-p 'read_verilog -defer $^' \
		-p 'prep -flatten -top lockbox -nordff' \
		-p 'write_smt2 -stdt lockbox.smt2'

%.rkt: %.smt2
	echo '#lang yosys' > $@
	cat $< >> $@
