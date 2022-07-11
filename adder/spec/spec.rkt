#lang knox/spec

#:init s0
#:symbolic-constructor new-symbolic-state
#:methods
(add [x (bitvector WIDTH)] [y (bitvector WIDTH)])
#:leak #f

(define WIDTH 32)

;; stateless
(define (new-symbolic-state)
  (void))

(define s0 (void))

(define ((add x y) s)
  (result (bvadd x y) s))
