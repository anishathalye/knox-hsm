#lang knox/circuit

#:circuit "pin.rkt"
#:reset resetn #f
#:persistent [wrapper.soc.rom.rom wrapper.soc.fram.fram]
#:init-zeroed [wrapper.soc.fram.fram]
