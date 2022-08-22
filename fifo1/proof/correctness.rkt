#lang knox/correctness

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:driver "../spec/driver.rkt"
#:R R
#:hints hints
#:verbose #t
;; #:only 'pop
;; #:without-crashes #t

(require
 "shared.rkt"
 knox/correctness/hint
 rosette/safe
 rosutil
 racket/match)

(define (hints method c1 f1 f-out f2)
  (match method
    ['(pop)
     (make-hintdb
      [split-has-data
       (tactic
        (define have-data (equal? (get-field c1 'have_data) (bv 1 1)))
        (case-split! (list have-data (! have-data)))
        (concretize! (lens 'circuit 'have_data)))])]
    [_ #f]))
