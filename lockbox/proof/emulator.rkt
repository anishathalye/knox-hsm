#lang knox/emulator

;; an example of an emulator that's written "from scratch", that
;; doesn't rely on a circuit copy at all

(struct state (en op secret password output))

(define (init)
  (set! (state #f #f (bv 0 128) (bv 0 128) (bv 0 128))))

(define (with-input i)
  (set! (state (input-en i) (input-op i) (input-secret i) (input-password i) (state-output (get)))))

(define (get-output)
  (output (state-output (get))))

(define (step)
  (let ([s (get)])
    (if (state-en s)
        (if (state-op s)
            (begin
              (spec:store (state-secret s) (state-password s))
              (set!
               (state
                (state-en s)
                (state-op s)
                (state-secret s)
                (state-password s)
                (bv 0 128))))
            (set!
             (state
              (state-en s)
              (state-op s)
              (state-secret s)
              (state-password s)
              (spec:get (state-password s)))))
        (set!
         (state
          (state-en s)
          (state-op s)
          (state-secret s)
          (state-password s)
          (bv 0 128))))))
