.PHONY: all
all: otp.rkt

SOC_V := $(shell find ../impl/hw/soc -name '*.v')

MEM := ../impl/fw/otp.mem

.PHONY: FORCE

$(MEM): FORCE
	$(MAKE) -C ../impl/ fw/otp.mem

otp.smt2: ../impl/hw/otp.v $(SOC_V) $(MEM)
	cd ../impl; \
	yosys \
		-p "read_verilog -defer $< $$(find hw/soc -name '*.v' | tr '\n' ' ')" \
		-p 'prep -flatten -top otp -nordff' \
		-p 'write_smt2 -stdt ../proof/otp.smt2'

otp.rkt: otp.smt2
	echo '#lang yosys' > $@
	cat $< >> $@

.PHONY: clean
clean:
	rm -f otp.rkt otp.smt2
