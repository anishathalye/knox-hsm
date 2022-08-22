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
    ['(peek)
     (make-hintdb
      [split-has-data
       (tactic
        (define empty (equal? (get-field c1 'head) (get-field c1 'tail)))
        (case-split! (list empty (! empty))))])]
    [_ #f]))
