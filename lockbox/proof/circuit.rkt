#lang knox/circuit

#:circuit "lockbox.rkt"
#:reset resetn #f
#:persistent [stored_secret stored_password]
#:init-zeroed [stored_secret stored_password]
