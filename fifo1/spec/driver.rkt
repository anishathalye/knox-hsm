#lang knox/driver

#:idle [rd #f wr #f]

(define (full)
  (output-full (in)))

(define (empty)
  (output-empty (in)))

(define (push v)
  (out* 'rd #f 'wr #t 'data_in v)
  (tick))

(define (pop)
  (hint split-has-data)
  (let ([o (in)])
    (let ([v (if (output-empty o) #f (output-data_out o))])
      (out* 'rd #t 'wr #f)
      (tick)
      v)))
