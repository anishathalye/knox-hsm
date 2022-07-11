#lang rosette/safe

(require
 (prefix-in spec: "../spec/spec.rkt")
 yosys/generic
 rosutil)

(provide (rename-out [R+ R]))

(define (boolean->bitvector b)
  (if b (bv 1 1) (bv 0 1)))

(define (R f ci)
  (define row0 (spec:state-row0 f))
  (define row1 (spec:state-row1 f))
  (and
   ;; tags can't match if both are valid
   (not (and
         (spec:row-valid row0)
         (spec:row-valid row1)
         (equal? (spec:row-tag row0) (spec:row-tag row1))))
   ;; valid bits match
   (equal? (get-field ci 'row_valid) (vector (boolean->bitvector (spec:row-valid row0)) (boolean->bitvector (spec:row-valid row1))))
   (if (spec:row-valid row0)
       (and
        (equal? (vector-ref (get-field ci 'tags) 0) (spec:row-tag row0))
        (equal? (vector-ref (get-field ci 'secret) 0) (spec:row-secret row0))
        (equal? (vector-ref (get-field ci 'password) 0) (spec:row-password row0)))
       (equal? row0 spec:row-empty))
   (if (spec:row-valid row1)
       (and
        (equal? (vector-ref (get-field ci 'tags) 1) (spec:row-tag row1))
        (equal? (vector-ref (get-field ci 'secret) 1) (spec:row-secret row1))
        (equal? (vector-ref (get-field ci 'password) 1) (spec:row-password row1)))
       (equal? row1 spec:row-empty))))

(define (I ci)
  (and
   (bvzero? (get-field ci 'returned_value))
   (bvzero? (get-field ci 'output_valid))
   (equal? (get-field ci 'state) (bv 0 2))))

(define (R+ f ci)
  (and (R f ci) (I ci)))
