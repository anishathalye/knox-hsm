#lang rosette/safe

(require (prefix-in ! racket/base))

(provide sha1
         (struct-out sha1-state)
         pad-message
         sha1-init
         sha1-finalize
         string->bits
         message->blocks
         f K)

;; FIPS 180-2

(define (pad-message m)
  (define l (bitvector-size (type-of m)))
  (define k (modulo (- 448 l 1) 512))
  (concat m (bv #b1 1) (bv 0 k) (bv l 64)))

(define (message->blocks M)
  (define w (bitvector-size (type-of M)))
  (let loop ([acc '()]
             [i 511])
    (if (i . > . w)
        acc
        (loop (cons (extract i (- i 511) M) acc)
              (+ i 512)))))

(struct sha1-state (a b c d e))

(define H0 (bv #x67452301 32))
(define H1 (bv #xefcdab89 32))
(define H2 (bv #x98badcfe 32))
(define H3 (bv #x10325476 32))
(define H4 (bv #xc3d2e1f0 32))

(define sha1-init (sha1-state H0 H1 H2 H3 H4))

(define (sha1-finalize s)
  (concat
   (sha1-state-a s)
   (sha1-state-b s)
   (sha1-state-c s)
   (sha1-state-d s)
   (sha1-state-e s)))

(define (f t x y z)
  (cond
    [(< t 20) (bvxor (bvand x y) (bvand (bvnot x) z))]
    [(< t 40) (bvxor x y z)]
    [(< t 60) (bvxor (bvand x y) (bvand x z) (bvand y z))]
    [(< t 80) (bvxor x y z)]))

(define (K t)
  (cond
    [(< t 20) (bv #x5a827999 32)]
    [(< t 40) (bv #x6ed9eba1 32)]
    [(< t 60) (bv #x8f1bbcdc 32)]
    [(< t 80) (bv #xca62c1d6 32)]))

(define (sha1-process-block s M)
  (define W (!build-vector 80 (lambda (_) (bv 0 32))))
  (!for ([t (!in-range 0 16)])
        (define l (* 32 (- 15 t)))
        (define h (+ l 31))
        (vector-set! W t (extract h l M)))
  (!for ([t (!in-range 16 80)])
        (vector-set! W t
                     (bvrol
                      (bvxor
                       (vector-ref W (- t 3))
                       (vector-ref W (- t 8))
                       (vector-ref W (- t 14))
                       (vector-ref W (- t 16)))
                      (bv 1 32))))
  ;; initialize working variables
  (define a (sha1-state-a s))
  (define b (sha1-state-b s))
  (define c (sha1-state-c s))
  (define d (sha1-state-d s))
  (define e (sha1-state-e s))
  ;; main loop
  (!for ([t (!in-range 80)])
        (define T (bvadd
                   (bvrol a (bv 5 32))
                   (f t b c d)
                   e
                   (K t)
                   (vector-ref W t)))
        (set! e d)
        (set! d c)
        (set! c (bvrol b (bv 30 32)))
        (set! b a)
        (set! a T))
  ;; compute next hash value
  (sha1-state
   (bvadd a (sha1-state-a s))
   (bvadd b (sha1-state-b s))
   (bvadd c (sha1-state-c s))
   (bvadd d (sha1-state-d s))
   (bvadd e (sha1-state-e s))))

(define (sha1-padded-blocks blocks)
  (let loop ([s sha1-init]
             [b blocks])
    (if (empty? b)
        (sha1-finalize s)
        (loop (sha1-process-block s (car b))
              (cdr b)))))

(define (sha1 m)
  (define M (pad-message m))
  (define blocks (message->blocks M))
  (sha1-padded-blocks blocks))

(define (string->bits str)
  (apply
   concat
   (map
    (lambda (c)
      (bv (!char->integer c) 8))
    (!string->list str))))

(define (sha1-string str)
  (sha1 (string->bits str)))

(module+ test
  (require rackunit)

  (test-case "small"
    (check-equal?
     (sha1-string "abc")
     (bv #xa9993e364706816aba3e25717850c26c9cd0d89d 160)))

  (test-case "multi block"
    (check-equal?
     (sha1-string "asdoifjasdjiasifjasdfsdg908fdaugdsfug030854tj0dsafg0dsiuf9234utuaj0dfjudsf9g9sfdgu9sdfu834ut009fsd")
     (bv #x942a2f206d8a0085fdf5e37640741c71f1f202ba 160))))
