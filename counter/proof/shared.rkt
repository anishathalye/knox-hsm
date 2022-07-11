#lang rosette/safe

(require rosutil)

(provide R)

(define (R spec circuit)
  (equal? spec (get-field circuit 'counter)))
