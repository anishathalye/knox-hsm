#lang knox/spec

#:init s0
#:symbolic-constructor new-symbolic-state
#:methods
(add [x (bitvector WIDTH)])
(get)
#:leak #f

(require rosutil)

(define WIDTH 32)

(define (new-symbolic-state)
  (fresh-symbolic 'counter (bitvector 32)))

(define s0 (bv 0 32))

(define ((add x) s)
  (if (bvule (bvadd (zero-extend x (bitvector 33)) (zero-extend s (bitvector 33))) (bv #xffffffff 33))
      (result (void) (bvadd x s))
      (result (void) (bv -1 32))))

(define ((get) s)
  (result s s))
