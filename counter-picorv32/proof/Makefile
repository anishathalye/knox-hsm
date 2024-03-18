.PHONY: all
all: counter.rkt

SOC_V := $(shell find ../impl/hw/soc -name '*.v')

MEM := ../impl/fw/counter.mem

.PHONY: FORCE

$(MEM): FORCE
	$(MAKE) -C ../impl/ fw/counter.mem

counter.smt2: ../impl/hw/counter.v $(SOC_V) $(MEM)
	cd ../impl; \
	yosys \
		-p "read_verilog -defer $< $$(find hw/soc -name '*.v' | tr '\n' ' ')" \
		-p 'prep -flatten -top counter -nordff' \
		-p 'write_smt2 -stdt ../proof/counter.smt2'

counter.rkt: counter.smt2
	echo '#lang yosys' > $@
	cat $< >> $@

.PHONY: clean
clean:
	rm -f counter.rkt counter.smt2
