#lang rosette/safe

(require rosutil
         "../spec/spec-sha1.rkt"
         "../spec/spec-hmac-sha1.rkt"
         (prefix-in spec: "../spec/spec.rkt")
         (only-in racket/base for/list in-range)
         (only-in racket/list range))

(provide AbsF R I swap32
         hmac-sha1-arg1
         hmac-sha1-arg2
         (struct-out imp-state)
         imp-init imp-next imp-step imp-finalize impl-spec-lenses)

(define (swap32 b)
  (concat
   (extract 7 0 b)
   (extract 15 8 b)
   (extract 23 16 b)
   (extract 31 24 b)))

(define (AbsF ci)
  (define fram (get-field ci 'wrapper.soc.fram.fram))
  (define active (vector-ref fram 0))
  (define active0 (bvzero? active))
  (define secret
    (concat
     (swap32 (if active0 (vector-ref fram 2) (vector-ref fram 10)))
     (swap32 (if active0 (vector-ref fram 3) (vector-ref fram 11)))
     (swap32 (if active0 (vector-ref fram 4) (vector-ref fram 12)))
     (swap32 (if active0 (vector-ref fram 5) (vector-ref fram 13)))
     (swap32 (if active0 (vector-ref fram 6) (vector-ref fram 14)))))
  (define max-ctr
    (concat
     (if active0 (vector-ref fram 9) (vector-ref fram 17))
     (if active0 (vector-ref fram 8) (vector-ref fram 16))))
  (spec:state secret max-ctr))

(define (R f ci)
  (&& (equal? (AbsF ci) f)
      (I ci)))

(define (I ci)
  (define fram (get-field ci 'wrapper.soc.fram.fram))
  (and
   (bveq (get-field ci 'wrapper.pwrmgr_state) (bv #b01 2))
   (equal? (get-field ci 'resetn) #t)))

;; proof tools

(define (hmac-sha1-arg1 key msg)
  (define key* (concat key (bv 0 (* 8 (- BLOCK-SIZE KEY-SIZE)))))
  (define i-key-pad (bvxor key* IPAD))
  (concat i-key-pad msg))

(define (hmac-sha1-arg2 first-sha1-imp key)
  (define key* (concat key (bv 0 (* 8 (- BLOCK-SIZE KEY-SIZE)))))
  (define o-key-pad (bvxor key* OPAD))
  (concat o-key-pad (imp-finalize first-sha1-imp)))

(addressable-struct imp-state (t a b c d e h0 h1 h2 h3 h4 W))

(define (imp-init* s M)
  (imp-state
   0
   (sha1-state-a s)
   (sha1-state-b s)
   (sha1-state-c s)
   (sha1-state-d s)
   (sha1-state-e s)
   (sha1-state-a s)
   (sha1-state-b s)
   (sha1-state-c s)
   (sha1-state-d s)
   (sha1-state-e s)
   (for/list ([t (in-range 0 16)])
     (define l (* 32 (- 15 t)))
     (define h (+ l 31))
     (extract h l M))))

(define (imp-init M)
  (imp-init* sha1-init M))

(define (imp-next s M)
  (imp-init* (imp->sha1-state s) M))

(define (imp-step s)
  (define t (imp-state-t s))
  (define a (imp-state-a s))
  (define b (imp-state-b s))
  (define c (imp-state-c s))
  (define d (imp-state-d s))
  (define e (imp-state-e s))
  (define h0 (imp-state-h0 s))
  (define h1 (imp-state-h1 s))
  (define h2 (imp-state-h2 s))
  (define h3 (imp-state-h3 s))
  (define h4 (imp-state-h4 s))
  (define W (imp-state-W s))
  (if (< t 80)
      (let ([w-next (bvrol
                     (bvxor
                      (list-ref W 13)
                      (list-ref W 8)
                      (list-ref W 2)
                      (list-ref W 0))
                     (bv 1 32))]
            [T (bvadd
                (bvrol a (bv 5 32))
                (f t b c d)
                e
                (K t)
                (list-ref W 0))])
        (imp-state
         (add1 t)
         T
         a
         (bvrol b (bv 30 32))
         c
         d
         h0
         h1
         h2
         h3
         h4
         (append (cdr W) (list w-next))))
      (imp-state
       (add1 t)
       a
       b
       c
       d
       e
       (bvadd a h0)
       (bvadd b h1)
       (bvadd c h2)
       (bvadd d h3)
       (bvadd e h4)
       W)))

(define (imp->sha1-state imp)
  (apply sha1-state (join-contents (lens-view (lens (list 'h0 'h1 'h2 'h3 'h4)) imp))))

(define (imp-finalize imp)
  (sha1-finalize (imp->sha1-state imp)))

(module+ test
  (require rackunit
           (only-in racket/base for/fold in-range))
  (test-case "imp <-> sha correspondence"
    (define-symbolic* msg (bitvector 326))
    (define M (first (message->blocks (pad-message msg))))
    (define imp0 (imp-init M))
    (define imp-final (for/fold ([imp imp0]) ([i (in-range 81)]) (imp-step imp)))
    (define imp-hash (imp-finalize imp-final))
    (define spec-hash (sha1 msg))
    (check-true (equal? imp-hash spec-hash))))

(define impl-spec-lenses
  (list (list 'a (lens 'wrapper.soc.sha1.a) (lens 'a))
        (list 'b (lens 'wrapper.soc.sha1.b) (lens 'b))
        (list 'c (lens 'wrapper.soc.sha1.c) (lens 'c))
        (list 'd (lens 'wrapper.soc.sha1.d) (lens 'd))
        (list 'e (lens 'wrapper.soc.sha1.e) (lens 'e))
        (list 'h0 (lens 'wrapper.soc.sha1.h0) (lens 'h0))
        (list 'h1 (lens 'wrapper.soc.sha1.h1) (lens 'h1))
        (list 'h2 (lens 'wrapper.soc.sha1.h2) (lens 'h2))
        (list 'h3 (lens 'wrapper.soc.sha1.h3) (lens 'h3))
        (list 'h4 (lens 'wrapper.soc.sha1.h4) (lens 'h4))
        (list '|w[0]| (lens 'wrapper.soc.sha1.w_mem 0) (lens 'W (list-ref-lens 0)))
        (list '|w[1]| (lens 'wrapper.soc.sha1.w_mem 1) (lens 'W (list-ref-lens 1)))
        (list '|w[2]| (lens 'wrapper.soc.sha1.w_mem 2) (lens 'W (list-ref-lens 2)))
        (list '|w[3]| (lens 'wrapper.soc.sha1.w_mem 3) (lens 'W (list-ref-lens 3)))
        (list '|w[4]| (lens 'wrapper.soc.sha1.w_mem 4) (lens 'W (list-ref-lens 4)))
        (list '|w[5]| (lens 'wrapper.soc.sha1.w_mem 5) (lens 'W (list-ref-lens 5)))
        (list '|w[6]| (lens 'wrapper.soc.sha1.w_mem 6) (lens 'W (list-ref-lens 6)))
        (list '|w[7]| (lens 'wrapper.soc.sha1.w_mem 7) (lens 'W (list-ref-lens 7)))
        (list '|w[8]| (lens 'wrapper.soc.sha1.w_mem 8) (lens 'W (list-ref-lens 8)))
        (list '|w[9]| (lens 'wrapper.soc.sha1.w_mem 9) (lens 'W (list-ref-lens 9)))
        (list '|w[10]| (lens 'wrapper.soc.sha1.w_mem 10) (lens 'W (list-ref-lens 10)))
        (list '|w[11]| (lens 'wrapper.soc.sha1.w_mem 11) (lens 'W (list-ref-lens 11)))
        (list '|w[12]| (lens 'wrapper.soc.sha1.w_mem 12) (lens 'W (list-ref-lens 12)))
        (list '|w[13]| (lens 'wrapper.soc.sha1.w_mem 13) (lens 'W (list-ref-lens 13)))
        (list '|w[14]| (lens 'wrapper.soc.sha1.w_mem 14) (lens 'W (list-ref-lens 14)))
        (list '|w[15]| (lens 'wrapper.soc.sha1.w_mem 15) (lens 'W (list-ref-lens 15)))))
