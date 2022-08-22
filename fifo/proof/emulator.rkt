#lang knox/emulator

(define (init)
  (void))

(define (with-input i)
  (set! i))

(define (get-output)
  (output* 'full (spec:full)
           'empty (spec:empty)
           'data_out (or (spec:peek) (bv 0 32))))

(define (step)
  (if (get-field (get) 'wr)
      (spec:push (get-field (get) 'data_in))
      (if (get-field (get) 'rd)
          (spec:pop)
          (void))))
