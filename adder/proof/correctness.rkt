#lang knox/correctness

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:driver "../spec/driver.rkt"
#:R R
#:hints hints
#:verbose #t

(require "shared.rkt"
         (only-in rosutil lens)
         knox/correctness/hint)

(define (hints method c1 f1 f-out f2)
  (make-hintdb
   [fp (fixpoint 0 #t 0 #f (lens "count_cycle"))]))
