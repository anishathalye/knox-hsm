#lang knox/circuit

#:circuit "lockbox.rkt"
#:reset i_rst_n #f
#:persistent [row_valid tags secret password]
#:init-zeroed [row_valid tags secret password]
