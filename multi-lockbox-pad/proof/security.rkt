#lang knox/security

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:emulator "emulator.rkt"
#:R R
;; #:skip-final-check #t

(require "shared.rkt" rosutil rosette/safe racket/match (prefix-in spec: "../spec/spec.rkt")
         (only-in racket/base for in-range))

(define circuit-all-fields-lens
  (lens (list (field-filter/not (field-filter/or 'row_valid 'tags 'secret 'password))
              (lens 'row_valid vector-all-elements-lens)
              (lens 'tags vector-all-elements-lens)
              (lens 'secret vector-all-elements-lens)
              (lens 'password vector-all-elements-lens))))
(define concretize-all-circuits-lens
  (lens (list (lens 'circuit circuit-all-fields-lens)
              (lens 'emulator 'auxiliary circuit-all-fields-lens))))
 (define sim-overapproximate-lens
 (lens 'emulator 'auxiliary
       (list #rx"^i_.*"
             #rx"^saved_.*"
             "search_index"
             "store_pass"
             "found_index"
             (lens 'row_valid (list 0 1))
             (lens 'tags (list 0 1))
             (lens 'secret (list 0 1))
             (lens 'password (list 0 1)))))
(define (bitvector->boolean b)
  (bveq b (bv 1 1)))

(define (subsumed-in! steps pos)
  (concretize! concretize-all-circuits-lens #:piecewise #t)
  (step!)
  (if (zero? steps)
      (begin
        (concretize! concretize-all-circuits-lens #:piecewise #t)
        (subsumed! pos))
      (subsumed-in! (sub1 steps) pos)))

(concretize! (lens 'circuit (list 'state 'returned_value 'output_valid)) #:piecewise #t)
(overapproximate! sim-overapproximate-lens)
;; replace spec state with AF(circuit)
(define (sync-circuit-oracle!)
  (let* ([ckt (lens-view (lens 'term 'circuit) (current))]
         [valid0 (bitvector->boolean (lens-view (lens 'row_valid 0) ckt))]
         [valid1 (bitvector->boolean (lens-view (lens 'row_valid 1) ckt))])
    (replace! (lens 'emulator 'oracle 'row0)
              (spec:row valid0
                        (if valid0 (lens-view (lens 'tags 0) ckt) (bv 0 spec:TAG-WIDTH))
                        (if valid0 (lens-view (lens 'secret 0) ckt) (bv 0 spec:WIDTH))
                        (if valid0 (lens-view (lens 'password 0) ckt) (bv 0 spec:WIDTH))))
    (replace! (lens 'emulator 'oracle 'row1)
              (spec:row valid1
                        (if valid1 (lens-view (lens 'tags 1) ckt) (bv 0 spec:TAG-WIDTH))
                        (if valid1 (lens-view (lens 'secret 1) ckt) (bv 0 spec:WIDTH))
                        (if valid1 (lens-view (lens 'password 1) ckt) (bv 0 spec:WIDTH))))
    (overapproximate-predicate!
     (! (&& valid0 valid1 (equal? (lens-view (lens 'tags 0) ckt)
                                  (lens-view (lens 'tags 1) ckt)))))))
(sync-circuit-oracle!)

(step!)

(displayln "case split on whether we got an input")

;; case analysis on whether we got an input
(cases!
 (let* ([t (set-term (current))]
        [en (lens-view (lens 'circuit 'i_en) t)])
   (list (! en) en)))

(subsumed! 0)

(concretize! concretize-all-circuits-lens #:piecewise #t)

;; clean up saved_* in circuit and emulator
(let ([ckt (lens-view (lens 'term 'circuit) (current))])
  ;; circuit
  (replace! (lens 'circuit 'saved_password) (lens-view (lens 'i_password) ckt))
  (replace! (lens 'circuit 'saved_secret) (lens-view (lens 'i_secret) ckt))
  (replace! (lens 'circuit 'saved_tag) (lens-view (lens 'i_tag) ckt))
  ;; emulator
  (replace! (lens 'emulator 'auxiliary 'saved_password) (lens-view (lens 'i_password) ckt))
  (replace! (lens 'emulator 'auxiliary 'saved_secret) (lens-view (lens 'i_secret) ckt))
  (replace! (lens 'emulator 'auxiliary 'saved_tag) (lens-view (lens 'i_tag) ckt)))


;; en=1, now we want to case split based on the op

(displayln "case split on op")

(cases!
 (let* ([t (set-term (current))]
        [op (lens-view (lens 'circuit 'i_op) t)])
   (list (! op) op)))

;; op=0 (get) case
(subsumed-in! 4 0)
(displayln "finished get case")

;; op=1 (store) case
(for ([_ (in-range 8)])
  (concretize! concretize-all-circuits-lens #:piecewise #t)
  (step!))
(overapproximate! sim-overapproximate-lens)
(sync-circuit-oracle!)
(subsumed! 0)
(displayln "finished store case")
