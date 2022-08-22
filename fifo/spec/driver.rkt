#lang knox/driver

#:idle [rd #f wr #f]

(define (full)
  (output-full (in)))

(define (empty)
  (output-empty (in)))

(define (push v)
  (out* 'rd #f 'wr #t 'data_in v)
  (tick))

(define (peek)
  (hint split-has-data)
  (let ([o (in)])
    (if (output-empty o) #f (output-data_out o))))

(define (pop)
  (out* 'rd #t 'wr #f)
  (tick))
