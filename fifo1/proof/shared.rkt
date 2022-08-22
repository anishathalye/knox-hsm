#lang rosette/safe

(require rosutil)

(provide R)

(define (R f ci)
  (if f
      ;; have data
      (&&
       (equal? (get-field ci 'have_data) (bv 1 1))
       (equal? (get-field ci 'data) f))
      ;; no data
      (equal? (get-field ci 'have_data) (bv 0 1))))
