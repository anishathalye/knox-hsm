#lang rosette/safe

(require rosutil
         (only-in "pwhash.rkt" pwhash_i input* with-input step new-symbolic-pwhash_s)
         "../spec/spec-sha256.rkt"
         (only-in racket/base for/list in-range)
         (only-in racket/list range))

(provide AbsF R I swap32
         (struct-out imp-state) imp-init imp-step sha256-add impl-spec-lenses imp->sha256-state)

(define (swap32 b)
  (concat
   (extract 7 0 b)
   (extract 15 8 b)
   (extract 23 16 b)
   (extract 31 24 b)))

;; we have written this very carefully, and in conjunction with the security proof,
;; to make it so that (R f ci) simplifies/computes in Rosette rather than requiring a solver query
;; in the security proof
(define (AbsF ci)
  (define fram (get-field ci 'wrapper.soc.fram.fram))
  (define active (vector-ref fram 0))
  (define active0 (bvzero? active))
  (concat
   (swap32 (if active0 (vector-ref fram 1) (vector-ref fram 6)))
   (swap32 (if active0 (vector-ref fram 2) (vector-ref fram 7)))
   (swap32 (if active0 (vector-ref fram 3) (vector-ref fram 8)))
   (swap32 (if active0 (vector-ref fram 4) (vector-ref fram 9)))
   (swap32 (if active0 (vector-ref fram 5) (vector-ref fram 10)))))

(define (R f ci)
  (&& (equal? (AbsF ci) f)
      (I ci)))

(define (I ci)
  (define fram (get-field ci 'wrapper.soc.fram.fram))
  (and
   (bveq (get-field ci 'wrapper.pwrmgr_state) (bv #b01 2))))

;; for proof purposes

(addressable-struct imp-state (t a b c d e f g h W))

(define (imp-init s M)
  (imp-state
   0
   (sha256-state-a s)
   (sha256-state-b s)
   (sha256-state-c s)
   (sha256-state-d s)
   (sha256-state-e s)
   (sha256-state-f s)
   (sha256-state-g s)
   (sha256-state-h s)
   (for/list ([t (in-range 0 16)])
     (define l (* 32 (- 15 t)))
     (define h (+ l 31))
     (extract h l M))))

(define (imp-step s)
  (define t (imp-state-t s))
  (define a (imp-state-a s))
  (define b (imp-state-b s))
  (define c (imp-state-c s))
  (define d (imp-state-d s))
  (define e (imp-state-e s))
  (define f (imp-state-f s))
  (define g (imp-state-g s))
  (define h (imp-state-h s))
  (define W (imp-state-W s))

  (define w-next
    (bvadd
     (sig1 (list-ref W 14))
     (list-ref W 9)
     (sig0 (list-ref W 1))
     (list-ref W 0)))

  (define T1 (bvadd h (Sum1 e) (Ch e f g) (vector-ref K t) (list-ref W 0)))
  (define T2 (bvadd (Sum0 a) (Maj a b c)))
  (imp-state
   (add1 t)
   (bvadd T1 T2)
   a
   b
   c
   (bvadd d T1)
   e
   f
   g
   (append (cdr W) (list w-next))))

(define (sha256-add s1 s2)
  (sha256-state
   (bvadd (sha256-state-a s1) (sha256-state-a s2))
   (bvadd (sha256-state-b s1) (sha256-state-b s2))
   (bvadd (sha256-state-c s1) (sha256-state-c s2))
   (bvadd (sha256-state-d s1) (sha256-state-d s2))
   (bvadd (sha256-state-e s1) (sha256-state-e s2))
   (bvadd (sha256-state-f s1) (sha256-state-f s2))
   (bvadd (sha256-state-g s1) (sha256-state-g s2))
   (bvadd (sha256-state-h s1) (sha256-state-h s2))))

(define (imp->sha256-state imp)
  (apply sha256-state (join-contents (lens-view (lens (list 'a 'b 'c 'd 'e 'f 'g 'h)) imp))))

(define impl-spec-lenses
  (list (list 'a (lens 'wrapper.soc.sha256.a) (lens 'a))
        (list 'b (lens 'wrapper.soc.sha256.b) (lens 'b))
        (list 'c (lens 'wrapper.soc.sha256.c) (lens 'c))
        (list 'd (lens 'wrapper.soc.sha256.d) (lens 'd))
        (list 'e (lens 'wrapper.soc.sha256.e) (lens 'e))
        (list 'f (lens 'wrapper.soc.sha256.f) (lens 'f))
        (list 'g (lens 'wrapper.soc.sha256.g) (lens 'g))
        (list 'h (lens 'wrapper.soc.sha256.h) (lens 'h))
        (list '|w[0]| (lens 'wrapper.soc.sha256.w_mem 0) (lens 'W (list-ref-lens 0)))
        (list '|w[1]| (lens 'wrapper.soc.sha256.w_mem 1) (lens 'W (list-ref-lens 1)))
        (list '|w[2]| (lens 'wrapper.soc.sha256.w_mem 2) (lens 'W (list-ref-lens 2)))
        (list '|w[3]| (lens 'wrapper.soc.sha256.w_mem 3) (lens 'W (list-ref-lens 3)))
        (list '|w[4]| (lens 'wrapper.soc.sha256.w_mem 4) (lens 'W (list-ref-lens 4)))
        (list '|w[5]| (lens 'wrapper.soc.sha256.w_mem 5) (lens 'W (list-ref-lens 5)))
        (list '|w[6]| (lens 'wrapper.soc.sha256.w_mem 6) (lens 'W (list-ref-lens 6)))
        (list '|w[7]| (lens 'wrapper.soc.sha256.w_mem 7) (lens 'W (list-ref-lens 7)))
        (list '|w[8]| (lens 'wrapper.soc.sha256.w_mem 8) (lens 'W (list-ref-lens 8)))
        (list '|w[9]| (lens 'wrapper.soc.sha256.w_mem 9) (lens 'W (list-ref-lens 9)))
        (list '|w[10]| (lens 'wrapper.soc.sha256.w_mem 10) (lens 'W (list-ref-lens 10)))
        (list '|w[11]| (lens 'wrapper.soc.sha256.w_mem 11) (lens 'W (list-ref-lens 11)))
        (list '|w[12]| (lens 'wrapper.soc.sha256.w_mem 12) (lens 'W (list-ref-lens 12)))
        (list '|w[13]| (lens 'wrapper.soc.sha256.w_mem 13) (lens 'W (list-ref-lens 13)))
        (list '|w[14]| (lens 'wrapper.soc.sha256.w_mem 14) (lens 'W (list-ref-lens 14)))
        (list '|w[15]| (lens 'wrapper.soc.sha256.w_mem 15) (lens 'W (list-ref-lens 15)))))
