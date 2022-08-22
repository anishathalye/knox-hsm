#lang knox/circuit

#:circuit "fifo.rkt"
#:reset resetn #f
#:persistent [data have_data]
#:init-zeroed [data have_data]
