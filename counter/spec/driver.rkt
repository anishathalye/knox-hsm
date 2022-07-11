#lang knox/driver

#:idle [en #f]

(define (add x)
  (out* 'en #t 'x x)
  (tick))

(define (get)
  (output-out (in)))
