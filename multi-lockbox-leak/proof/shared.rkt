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
   (not (and
         (spec:row-valid row0)
         (spec:row-valid row1)
         (equal? (spec:row-tag row0) (spec:row-tag row1))))
   (equal? (get-field ci 'row_valid) (vector (boolean->bitvector (spec:row-valid row0)) (boolean->bitvector (spec:row-valid row1))))
   (if (spec:row-valid row0)
       (and
        (equal? (vector-ref (get-field ci 'tags) 0) (spec:row-tag row0))
        (equal? (vector-ref (get-field ci 'secret) 0) (spec:row-secret row0))
        (equal? (vector-ref (get-field ci 'password) 0) (spec:row-password row0)))
       #t)
   (if (spec:row-valid row1)
       (and
        (equal? (vector-ref (get-field ci 'tags) 1) (spec:row-tag row1))
        (equal? (vector-ref (get-field ci 'secret) 1) (spec:row-secret row1))
        (equal? (vector-ref (get-field ci 'password) 1) (spec:row-password row1)))
       #t)))

(define (I ci)
  (and
   (bvzero? (get-field ci 'returned_value))
   (bvzero? (get-field ci 'output_valid))
   (equal? (get-field ci 'state) (bv 0 2))))

(define (R+ f ci)
  (and (R f ci) (I ci)))
