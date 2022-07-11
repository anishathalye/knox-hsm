#lang knox/circuit

#:circuit "pwhash.rkt"
#:reset resetn #f
#:persistent [wrapper.soc.rom.rom wrapper.soc.fram.fram wrapper.soc.sha256.k]
#:init-zeroed [wrapper.soc.fram.fram]
