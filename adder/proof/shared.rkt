#lang rosette/safe

(require rosutil)

(provide R)

(define (R f ci)
  (I ci))

(define (I ci)
  (and
   (equal? (get-field ci 'state) (bv 0 2))
   (equal? (get-field ci 'out) (bv 0 32))))
