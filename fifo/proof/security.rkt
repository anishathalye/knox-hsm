#lang knox/security

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:emulator "emulator.rkt"
#:R R
;; #:skip-final-check #t

(require "shared.rkt" rosette/safe rosutil)

;; replace oracle with AbsF(circuit) and forget predicate
(replace! (lens 'emulator 'oracle)
          (AbsF (lens-view (lens 'term 'circuit) (current))))
(overapproximate-predicate! #t)

(step!)

;; simplify oracle state again
(replace! (lens 'emulator 'oracle)
          (AbsF (lens-view (lens 'term 'circuit) (current))))
;; now, the subsumption check will be fast
(subsumed! 0)
