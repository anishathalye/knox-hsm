#lang knox/security

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:emulator "emulator.rkt"
#:R R
;; #:skip-final-check #t

(require "shared.rkt" rosutil rosette/safe)

;; replace spec state with AbsF(circuit), so we can get rid of predicate
(define c0 (lens-view (lens 'term 'circuit) (current)))
(replace! (lens 'emulator 'oracle)
          (if (equal? (get-field c0 'have_data) (bv 1 1))
              (get-field c0 'data)
              #f))
;; line up emulator and circuit
(replace! (lens 'emulator 'auxiliary 'have_data)
          (get-field c0 'have_data))
;; but its data doesn't necessarily match
(replace! (lens 'emulator 'auxiliary 'data)
          (if (equal? (get-field c0 'have_data) (bv 1 1))
              (get-field c0 'data)
              (bv 0 32)))
(overapproximate-predicate! #t)
;; if emulator doesn't have data, it has a don't-care value
(overapproximate*! (lens 'emulator 'auxiliary 'data)
          (if (equal? (get-field c0 'have_data) (bv 1 1))
              (get-field c0 'data)
              (fresh-symbolic 'data_dummy (bitvector 32))))

(step!)
(subsumed! 0)
