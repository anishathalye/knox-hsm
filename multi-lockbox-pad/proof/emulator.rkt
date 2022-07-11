#lang knox/emulator

(define WIDTH 128)

(define (init)
  (set! (circuit-new)))

(define (with-input i)
  (set! (circuit-with-input (get) i)))

(define (get-output)
  (circuit-get-output (get)))

(define (step)
  ;; step dummy circuit
  ;;
  ;; if the circuit is giving "real" output, give output from oracle
  (let ([c2 (circuit-step (get))])
    (set!
     (if (output-o_valid (circuit-get-output c2))
         ;; call into oracle
         (let ([fixed-output (let ([op (get-field c2 'saved_op)]
                                   [tag (get-field c2 'saved_tag)]
                                   [password (get-field c2 'saved_password)]
                                   [secret (get-field c2 'saved_secret)])
                               (if (bveq op (bv 0 1))
                                   ;; get
                                   (spec:get tag password)
                                   ;; store
                                   (if (spec:store tag secret password) (bv 1 WIDTH) (bv 0 WIDTH))))])
           (update-field c2 'returned_value fixed-output))
         c2))))
