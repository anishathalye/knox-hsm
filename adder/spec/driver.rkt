#lang knox/driver

#:idle [en #f]

(define (add x y)
  (out* 'en #t 'x x)
  (tick)
  (out* 'en #f)
  (yield fp)
  (out* 'en #t 'x y)
  (tick)
  (out* 'en #f)
  (let ([r (output-out (in))])
    (tick)
    r))
