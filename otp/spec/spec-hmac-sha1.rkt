#lang rosette/safe

(require "spec-sha1.rkt")

(provide hmac-sha1 BLOCK-SIZE DIGEST-SIZE KEY-SIZE COUNTER-SIZE IPAD OPAD)

(define BLOCK-SIZE 64)
(define DIGEST-SIZE 20)
;; specialized to a 20 byte key and an 8 byte counter
(define KEY-SIZE 20)
(define COUNTER-SIZE 8)

(define (bitvector-repeat v n)
  (if (equal? n 1) v (concat v (bitvector-repeat v (sub1 n)))))

(define IPAD (bitvector-repeat (bv #x36 8) BLOCK-SIZE))
(define OPAD (bitvector-repeat (bv #x5c 8) BLOCK-SIZE))

(define (hmac-sha1 key msg)
  (define key* (concat key (bv 0 (* 8 (- BLOCK-SIZE KEY-SIZE)))))
  (define i-key-pad (bvxor key* IPAD))
  (define o-key-pad (bvxor key* OPAD))
  (sha1 (concat o-key-pad (sha1 (concat i-key-pad msg)))))

(module+ test
  (require rackunit)

  (test-case "RFC2202 test vector 1"
    (check-equal?
     (hmac-sha1 (bitvector-repeat (bv #x0b 8) KEY-SIZE) (string->bits "Hi There"))
     (bv #xb617318655057264e28bc0b6fb378c8ef146be00 160))))
