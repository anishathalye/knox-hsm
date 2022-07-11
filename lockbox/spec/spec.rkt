#lang knox/spec

#:init s0
#:symbolic-constructor new-symbolic-state
#:methods
(store [secret (bitvector WIDTH)] [password (bitvector WIDTH)])
(get [guess (bitvector WIDTH)])
#:leak #f

(require rosutil)

(define WIDTH 128)

(struct state (secret password))

(define s0
  (state (bv 0 WIDTH)
         (bv 0 WIDTH)))

(define (new-symbolic-state)
  (state
   (fresh-symbolic 'secret (bitvector WIDTH))
   (fresh-symbolic 'password (bitvector WIDTH))))

(define ((store secret password) s)
  (result #t (state secret password)))

(define ((get guess) s)
  (result
   (if (equal? guess (state-password s))
       (state-secret s)
       (bv 0 WIDTH))
   s0))
