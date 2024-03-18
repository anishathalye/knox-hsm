#lang rosette/safe

(require rosutil
         "../spec/spec.rkt"
         "counter.rkt"
         (only-in racket/list range))

(provide AbsF R I)

;; gives spec state corresponding to a circuit state
(define (AbsF ci)
  ;; sizeof(struct entry) == 24 bytes, which is 6 entries in the vector (of 32-bit words)
  (define fram (get-field ci 'wrapper.soc.fram.fram))
  (vector-ref fram 0))

(define (R f ci)
  (and
   (equal? (AbsF ci) f)
   (I ci)))

(define (I ci)
  (bveq (get-field ci 'wrapper.pwrmgr_state) (bv #b01 2)))
