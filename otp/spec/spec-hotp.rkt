#lang rosette/safe

(require "spec-hmac-sha1.rkt"
         (only-in "spec-sha1.rkt" string->bits))

(provide hotp
         dt
         SECRET-SIZE
         SECRET-SIZE-BYTES
         CTR-SIZE
         CTR-SIZE-BYTES
         DIGITS)

(define SECRET-SIZE (* 8 20))
(define SECRET-SIZE-BYTES 20)
(define CTR-SIZE (* 8 8))
(define CTR-SIZE-BYTES 8)
(define DIGITS 6)

(define (hotp key c)
  (define hs (hmac-sha1 key c))
  (define s (dt hs))
  (bvurem s (bv (expt 10 DIGITS) 32)))

(define (get-byte v offset)
  (cond
    [(equal? offset (bv 0 5)) (extract 159 152 v)]
    [(equal? offset (bv 1 5)) (extract 151 144 v)]
    [(equal? offset (bv 2 5)) (extract 143 136 v)]
    [(equal? offset (bv 3 5)) (extract 135 128 v)]
    [(equal? offset (bv 4 5)) (extract 127 120 v)]
    [(equal? offset (bv 5 5)) (extract 119 112 v)]
    [(equal? offset (bv 6 5)) (extract 111 104 v)]
    [(equal? offset (bv 7 5)) (extract 103 96 v)]
    [(equal? offset (bv 8 5)) (extract 95 88 v)]
    [(equal? offset (bv 9 5)) (extract 87 80 v)]
    [(equal? offset (bv 10 5)) (extract 79 72 v)]
    [(equal? offset (bv 11 5)) (extract 71 64 v)]
    [(equal? offset (bv 12 5)) (extract 63 56 v)]
    [(equal? offset (bv 13 5)) (extract 55 48 v)]
    [(equal? offset (bv 14 5)) (extract 47 40 v)]
    [(equal? offset (bv 15 5)) (extract 39 32 v)]
    [(equal? offset (bv 16 5)) (extract 31 24 v)]
    [(equal? offset (bv 17 5)) (extract 23 16 v)]
    [(equal? offset (bv 18 5)) (extract 15 8 v)]
    [(equal? offset (bv 19 5)) (extract 7 0 v)]
    [else (bv 0 8)])) ; never hit this case

(define (dt hs)
  (define offset (zero-extend (extract 3 0 hs) (bitvector 5)))
  (concat
   (bvand (get-byte hs offset) (bv #x7f 8))
   (get-byte hs (bvadd offset (bv 1 5)))
   (get-byte hs (bvadd offset (bv 2 5)))
   (get-byte hs (bvadd offset (bv 3 5)))))

(module+ test
  (require rackunit)

  (test-case "RFC4226 test vector 1"
    (check-equal?
     (hotp (string->bits "12345678901234567890") (bv 0 64))
     (bv 755224 32))))
