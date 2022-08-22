#lang knox/emulator

(define (init)
  (set!
   (let ([v (spec:pop)])
     (if v (spec:push v) (void)) ; put it back
     (update-fields (circuit-new)
                    (list
                     (cons 'have_data (if v (bv 1 1) (bv 0 1)))
                     (cons 'data (if v v (bv 0 32))))))))

(define (with-input i)
  (set! (circuit-with-input (get) i)))

(define (get-output)
  (circuit-get-output (get)))

(define (step)
  (if (&& (get-field (get) 'wr) (equal? (get-field (get) 'have_data) (bv 0 1)))
      (spec:push (get-field (get) 'data_in))
      (if (get-field (get) 'rd)
          (spec:pop)
          (void)))
  (set! (circuit-step (get))))
