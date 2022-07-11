.PHONY: all
all: pwhash.rkt

SOC_V := $(shell find ../impl/hw/soc -name '*.v')

MEM := ../impl/fw/pwhash.mem

.PHONY: FORCE

$(MEM): FORCE
	$(MAKE) -C ../impl/ fw/pwhash.mem

pwhash.smt2: ../impl/hw/pwhash.v $(SOC_V) $(MEM)
	cd ../impl; \
	yosys \
		-p "read_verilog -defer $< $$(find hw/soc -name '*.v' | tr '\n' ' ')" \
		-p 'prep -flatten -top pwhash -nordff' \
		-p 'write_smt2 -stdt ../proof/pwhash.smt2'

pwhash.rkt: pwhash.smt2
	echo '#lang yosys' > $@
	cat $< >> $@

.PHONY: clean
clean:
	rm -f pwhash.rkt pwhash.smt2
