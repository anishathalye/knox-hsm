#lang knox/correctness

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:driver "../spec/driver.rkt"
#:R R
#:hints hints
;; #:only 'store
;; #:without-crashes #t
#:verbose #t

(require "shared.rkt")

(require
 knox/correctness/hint
 rosutil
 (except-in rosette/safe result-state result-value result?)
 racket/match)

(define debug
  (tactic
   (define s (get-state))
   (println (lens-view (lens 'interpreter 'globals 'circuit) s))))
(define hint-concretize
  (concretize! (lens 'circuit (field-filter/or "output_valid" "state" "search_index" "store_pass")) #:use-pc #t))

(define (case-split* splits)
  (case-split!
   (let loop ([acc '()]
              [ps splits]
              [pc #t])
     (if (null? ps)
         (cons pc acc)
         (loop
          (cons (&& pc (car ps)) acc)
          (cdr ps)
          (&& pc (! (car ps))))))))

(define (hints method c1 f1 f-out f2)
  (match method
    [`(get ,tag ,guess)
     (make-hintdb
      [debug debug]
      [concretize hint-concretize]
      [cases-get-tag-match (case-split* (list
                                         ;; match index 0
                                         (and
                                          (equal? (vector-ref (get-field c1 'row_valid) 0) (bv 1 1))
                                          (equal? (vector-ref (get-field c1 'tags) 0) tag))
                                         ;; match index 1
                                         (and
                                          (equal? (vector-ref (get-field c1 'row_valid) 1) (bv 1 1))
                                          (equal? (vector-ref (get-field c1 'tags) 1) tag))))])]
    [`(store ,tag ,secret ,password)
     (make-hintdb
      [debug debug]
      [concretize hint-concretize]
      [cases-store-tag-match (case-split* (list
                                           ;; match index 0
                                           (and
                                            (equal? (vector-ref (get-field c1 'row_valid) 0) (bv 1 1))
                                            (equal? (vector-ref (get-field c1 'tags) 0) tag))
                                           ;; match index 1
                                           (and
                                            (equal? (vector-ref (get-field c1 'row_valid) 1) (bv 1 1))
                                            (equal? (vector-ref (get-field c1 'tags) 1) tag))
                                           ;; empty index 0
                                           (equal? (vector-ref (get-field c1 'row_valid) 0) (bv 0 1))
                                           ;; empty index 1
                                           (equal? (vector-ref (get-field c1 'row_valid) 1) (bv 0 1))))])]))
