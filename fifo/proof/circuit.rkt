#lang knox/circuit

#:circuit "fifo.rkt"
#:reset resetn #f
#:persistent [data head tail]
#:init-zeroed [data head tail]
