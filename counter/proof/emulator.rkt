#lang knox/emulator

(define (init)
  (let ([c (update-field
            (circuit-new)
            'counter
            (spec:get))])
    (set! c)))

(define (with-input i)
  (set! (circuit-with-input (get) i)))

(define (get-output)
  (circuit-get-output (get)))

(define (step)
  ;; need to keep spec in sync to satisfy crash property, even though
  ;; we don't need it to emulate outputs
  (if (get-field (get) 'en)
      (spec:add (get-field (get) 'x))
      (void))
  (set! (circuit-step (get))))
