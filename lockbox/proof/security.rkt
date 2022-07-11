#lang knox/security

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:emulator "emulator.rkt"
#:R R
;; #:skip-final-check #t

(require "shared.rkt" rosutil rosette/safe racket/match)

;; to set up subsumption for later: we care that returned_secret is
;; identical between circuit and emulator, but we don't care what the
;; value is (otherwise, in this initial state, it is known to be 0)

;; prove that they're equal
(replace! (lens 'emulator 'auxiliary 'output) (lens-view (lens 'circuit 'returned_secret) (set-term (current))))
;; abstract with a fresh variable
(define returned_secret (remember+! (list (lens 'emulator 'auxiliary 'output) (lens 'circuit 'returned_secret))))
;; don't need to remember the value
(clear! returned_secret)

(step!)
(subsumed! 0)
