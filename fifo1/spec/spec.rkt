#lang knox/spec

#:init s0
#:symbolic-constructor new-symbolic-state
#:methods
(full)
(empty)
(push [v (bitvector WIDTH)])
(pop)

(require rosutil)

(define WIDTH 32)

(define (new-symbolic-state)
  (define-symbolic* has-value boolean?)
  (if has-value
      (fresh-symbolic 'value (bitvector WIDTH))
      #f))

(define s0 #f)

(define ((full) s)
  (result (if s #t #f) s))

(define ((empty) s)
  (result (if s #f #t) s))

(define ((push v) s)
  (result (void) (or s v)))

(define ((pop) s)
  (result s #f))
