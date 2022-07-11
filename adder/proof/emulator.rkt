#lang knox/emulator

(define (init)
  (set! (circuit-new)))

(define (with-input i)
  (set! (circuit-with-input (get) i)))

(define (get-output)
  (circuit-get-output (get)))

(define (step)
  (set! (circuit-step (get))))
