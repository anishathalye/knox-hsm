#lang rosette/safe

(require rosutil "../spec/spec.rkt")

(provide AbsF R)

(define (AbsF ci)
  ;; finite loop bound to make this amenable to symbolic execution
  (define tail (get-field ci 'tail))
  (define data (get-field ci 'data))
  (let rec ([n CAPACITY]
            [ptr (get-field ci 'head)])
    (cond
      [(zero? n) (list)]
      [(bveq ptr tail) (list)]
      [else
       (cons
        (vector-ref-bv data ptr)
        (rec (sub1 n) (bvadd ptr (bv 1 CAPACITY-LOG2))))])))

(define (R f ci)
  (equal? f (AbsF ci)))
