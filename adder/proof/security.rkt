#lang knox/security

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:emulator "emulator.rkt"
#:R R
;; #:skip-final-check #t

(require "shared.rkt"
         rosutil
         rosette/safe)

;; don't care about cycle counter or accumulator
(overapproximate!
 (lens (list (lens 'circuit (list 'count_cycle 'acc))
             (lens 'emulator 'auxiliary (list 'count_cycle 'acc)))))

(define (step-en!)
  (define prev-index (length (visited)))
  (step!)
  ;; case split based on whether en was true
  (define en (lens-view (lens 'term 'circuit 'en) (current)))
  (cases! (list (! en) en))
  ;; when not en, nothing has changed
  (subsumed! prev-index))

(step-en!)
(step-en!)
(step!)
(subsumed! 0)
