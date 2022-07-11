#lang knox/security

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:emulator "emulator.rkt"
#:R R
;; #:skip-final-check #t

(require "shared.rkt" rosutil rosette/safe racket/match)

(define (cases*! preds)
  (cases! (append preds (list (not (apply || preds))))))

;; a workaround for concretize not supporting arbitrary structs yet
(define circuit-all-fields-lens
  (lens (list (field-filter/not (field-filter/or 'row_valid 'tags 'secret 'password))
              (lens 'row_valid vector-all-elements-lens)
              (lens 'tags vector-all-elements-lens)
              (lens 'secret vector-all-elements-lens)
              (lens 'password vector-all-elements-lens))))
(define concretize-all-circuits-lens
  (lens (list (lens 'circuit circuit-all-fields-lens)
              (lens 'emulator 'auxiliary 'circuit circuit-all-fields-lens))))
(define sim-overapproximate-lens
  (lens 'emulator 'auxiliary 'circuit
        (list #rx"^i_.*"
              #rx"^saved_.*"
              "search_index"
              "store_pass"
              (lens 'secret (list 0 1))
              (lens 'password (list 0 1)))))

(concretize! (lens 'circuit (list 'state 'returned_value 'output_valid)) #:piecewise #t)
(overapproximate! sim-overapproximate-lens)

(overapproximate*!
 (lens 'emulator 'auxiliary 'circuit 'tags (list 0 1))
 (let* ([t (set-term (current))]
        [spec (lens-view (lens 'emulator 'oracle (list (lens 'row0 (list 'valid 'tag))
                                                       (lens 'row1 (list 'valid 'tag)))) t)])
   (match-define (join (list (join (list valid0 tag0)) (join (list valid1 tag1)))) spec)
   (join*
    (if valid0 tag0 (fresh-symbolic 'tag0-unused (bitvector 16)))
    (if valid1 tag1 (fresh-symbolic 'tag1-unused (bitvector 16))))))

(step!)

(displayln "case split on whether we got an input")

;; case analysis on whether we got an input
(cases*!
 (let* ([t (set-term (current))]
        [en (lens-view (lens 'circuit 'i_en) t)])
   (list (not en))))

(subsumed! 0)

(concretize! concretize-all-circuits-lens #:piecewise #t)

;; en=1, now we want to case split based on the op

(displayln "case split on op")

(cases*!
 (let* ([t (set-term (current))]
        [op (lens-view (lens 'circuit 'i_op) t)])
   (list (not op))))

;; op=0 (get) case
(concretize! concretize-all-circuits-lens #:piecewise #t)
(cases*!
 (let* ([t (set-term (current))]
        [i-tag (lens-view (lens 'emulator 'auxiliary 'circuit 'saved_tag) t)]
        [spec (lens-view (lens 'emulator 'oracle (list (lens 'row0 (list 'valid 'tag))
                                                        (lens 'row1 (list 'valid 'tag)))) t)])
   (match-define (join (list (join (list valid0 tag0)) (join (list valid1 tag1)))) spec)
   (let* ([match0 (and valid0 (equal? tag0 i-tag))]
          [match1 (and (not match0) valid1 (equal? tag1 i-tag))])
     (list match0 match1))))

(define (subsumed-in steps pos)
  (concretize! concretize-all-circuits-lens #:piecewise #t)
  (step!)
  (if (zero? steps)
      (begin
        (concretize! concretize-all-circuits-lens #:piecewise #t)
        (subsumed! pos))
      (subsumed-in (sub1 steps) pos)))

(subsumed-in 2 0) ; match 0
(subsumed-in 3 0) ; match 1
(subsumed-in 4 0) ; match none

(displayln "finished get case")

;; op=1 (store) case
(concretize! concretize-all-circuits-lens #:piecewise #t)
(cases*!
 (let* ([t (set-term (current))]
        [i-tag (lens-view (lens 'emulator 'auxiliary 'circuit 'saved_tag) t)]
        [spec (lens-view (lens 'emulator 'oracle (list (lens 'row0 (list 'valid 'tag))
                                                        (lens 'row1 (list 'valid 'tag)))) t)])
   (match-define (join (list (join (list valid0 tag0)) (join (list valid1 tag1)))) spec)
   (let* ([match0 (and valid0 (equal? tag0 i-tag))]
          [match1 (and (not match0) valid1 (equal? tag1 i-tag))]
          [empty0 (and (not match0) (not match1) (not valid0))]
          [empty1 (and (not match0) (not match1) (not empty0) (not valid1))])
     (list match0 match1 empty0 empty1))))

(subsumed-in 2 0) ; match 0
(subsumed-in 3 0) ; match 1
(subsumed-in 5 0) ; empty 0
(subsumed-in 6 0) ; empty 1
(subsumed-in 7 0) ; full

(displayln "finished store case")
