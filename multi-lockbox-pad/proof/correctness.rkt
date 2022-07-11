#lang knox/correctness

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:driver "../spec/driver.rkt"
#:R R
;; #:only 'init
;; #:without-crashes #t
#:verbose #t

(require "shared.rkt")
