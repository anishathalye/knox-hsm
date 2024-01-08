#lang rosette/safe

(require rosutil
         "../spec/spec.rkt"
         "pin.rkt"
         (only-in racket/list range))

(provide AbsF R I)

(define (swap32 b)
  (concat
   (extract 7 0 b)
   (extract 15 8 b)
   (extract 23 16 b)
   (extract 31 24 b)))

;; gives spec state corresponding to a circuit state
(define (AbsF ci)
  ;; sizeof(struct entry) == 24 bytes, which is 6 entries in the vector (of 32-bit words)
  (define fram (get-field ci 'wrapper.soc.fram.fram))
  (define (Abs1 slot)
    (define idx (* slot 6))
    (define valid (not (bvzero? (extract 7 0 (vector-ref fram idx)))))
    (if (not valid)
        e0
        (entry
         valid
         (bvumin (extract 15 8 (vector-ref fram idx)) (bv GUESS-LIMIT 8))
         (swap32 (vector-ref fram (add1 idx)))
         (concat
          (swap32 (vector-ref fram (+ idx 2)))
          (swap32 (vector-ref fram (+ idx 3)))
          (swap32 (vector-ref fram (+ idx 4)))
          (swap32 (vector-ref fram (+ idx 5)))))))
  (map (lambda (slot) (Abs1 slot)) (range 0 NENTRY)))

(define (R f ci)
  (and
   (equal? (AbsF ci) f)
   (I ci)))

(define (I ci)
  (bveq (get-field ci 'wrapper.pwrmgr_state) (bv #b01 2)))
