#lang rosette/safe

(require (prefix-in ! racket))

;; spec value is 63, though we can reduce this to do a reduced round
;; sha256 (for testing purposes)
(define SHA256-ROUNDS 63)

(provide
 (struct-out sha256-state)
 Ch Maj Sum0 Sum1 sig0 sig1 K
 sha256-init
 sha256-process-block
 sha256-finalize
 pad-message
 message->blocks
 sha256-padded-blocks
 sha256
 string->bits
 sha256-string)

(define (Ch x y z)
  (bvxor
   (bvand x y)
   (bvand (bvnot x) z)))

(define (Maj x y z)
  (bvxor
   (bvand x y)
   (bvand x z)
   (bvand y z)))

(define (Sum0 x)
  (bvxor
   (bvror x (bv 2 32))
   (bvror x (bv 13 32))
   (bvror x (bv 22 32))))

(define (Sum1 x)
  (bvxor
   (bvror x (bv 6 32))
   (bvror x (bv 11 32))
   (bvror x (bv 25 32))))

(define (sig0 x)
  (bvxor
   (bvror x (bv 7 32))
   (bvror x (bv 18 32))
   (bvlshr x (bv 3 32))))

(define (sig1 x)
  (bvxor
   (bvror x (bv 17 32))
   (bvror x (bv 19 32))
   (bvlshr x (bv 10 32))))

(define K
  (vector (bv #x428a2f98 32) (bv #x71374491 32) (bv #xb5c0fbcf 32) (bv #xe9b5dba5 32) (bv #x3956c25b 32) (bv #x59f111f1 32) (bv #x923f82a4 32) (bv #xab1c5ed5 32)
          (bv #xd807aa98 32) (bv #x12835b01 32) (bv #x243185be 32) (bv #x550c7dc3 32) (bv #x72be5d74 32) (bv #x80deb1fe 32) (bv #x9bdc06a7 32) (bv #xc19bf174 32)
          (bv #xe49b69c1 32) (bv #xefbe4786 32) (bv #x0fc19dc6 32) (bv #x240ca1cc 32) (bv #x2de92c6f 32) (bv #x4a7484aa 32) (bv #x5cb0a9dc 32) (bv #x76f988da 32)
          (bv #x983e5152 32) (bv #xa831c66d 32) (bv #xb00327c8 32) (bv #xbf597fc7 32) (bv #xc6e00bf3 32) (bv #xd5a79147 32) (bv #x06ca6351 32) (bv #x14292967 32)
          (bv #x27b70a85 32) (bv #x2e1b2138 32) (bv #x4d2c6dfc 32) (bv #x53380d13 32) (bv #x650a7354 32) (bv #x766a0abb 32) (bv #x81c2c92e 32) (bv #x92722c85 32)
          (bv #xa2bfe8a1 32) (bv #xa81a664b 32) (bv #xc24b8b70 32) (bv #xc76c51a3 32) (bv #xd192e819 32) (bv #xd6990624 32) (bv #xf40e3585 32) (bv #x106aa070 32)
          (bv #x19a4c116 32) (bv #x1e376c08 32) (bv #x2748774c 32) (bv #x34b0bcb5 32) (bv #x391c0cb3 32) (bv #x4ed8aa4a 32) (bv #x5b9cca4f 32) (bv #x682e6ff3 32)
          (bv #x748f82ee 32) (bv #x78a5636f 32) (bv #x84c87814 32) (bv #x8cc70208 32) (bv #x90befffa 32) (bv #xa4506ceb 32) (bv #xbef9a3f7 32) (bv #xc67178f2 32)))

(define (pad-message m)
  (define l (bitvector-size (type-of m)))
  (define k (modulo (- 448 l 1) 512))
  (concat m (bv #b1 1) (bv 0 k) (bv l 64)))

(define H0 (bv #x6a09e667 32))
(define H1 (bv #xbb67ae85 32))
(define H2 (bv #x3c6ef372 32))
(define H3 (bv #xa54ff53a 32))
(define H4 (bv #x510e527f 32))
(define H5 (bv #x9b05688c 32))
(define H6 (bv #x1f83d9ab 32))
(define H7 (bv #x5be0cd19 32))

(struct sha256-state (a b c d e f g h))

(define (sha256-init) (sha256-state H0 H1 H2 H3 H4 H5 H6 H7))

;; s is a sha256-state
;; b is a (bitvector 512)
(define (sha256-process-block s M)
  ;; prepare message schedule
  (define W (!build-vector 64 (lambda (_) (bv 0 32))))
  (!for ([t (!in-range 0 16)])
        (define l (* 32 (- 15 t)))
        (define h (+ l 31))
        (vector-set! W t (extract h l M)))
  (!for ([t (!in-range 16 64)])
        (vector-set! W t (bvadd
                          (sig1 (vector-ref W (- t 2)))
                          (vector-ref W (- t 7))
                          (sig0 (vector-ref W (- t 15)))
                          (vector-ref W (- t 16)))))
  ;; initialize working variables
  (define a (sha256-state-a s))
  (define b (sha256-state-b s))
  (define c (sha256-state-c s))
  (define d (sha256-state-d s))
  (define e (sha256-state-e s))
  (define f (sha256-state-f s))
  (define g (sha256-state-g s))
  (define h (sha256-state-h s))
  ;; main loop
  (!for ([t (!in-range 0 (add1 SHA256-ROUNDS))])
        (define T1 (bvadd h (Sum1 e) (Ch e f g) (vector-ref K t) (vector-ref W t)))
        (define T2 (bvadd (Sum0 a) (Maj a b c)))
        (set! h g)
        (set! g f)
        (set! f e)
        (set! e (bvadd d T1))
        (set! d c)
        (set! c b)
        (set! b a)
        (set! a (bvadd T1 T2)))
  ;; compute next hash value
  (sha256-state
   (bvadd a (sha256-state-a s))
   (bvadd b (sha256-state-b s))
   (bvadd c (sha256-state-c s))
   (bvadd d (sha256-state-d s))
   (bvadd e (sha256-state-e s))
   (bvadd f (sha256-state-f s))
   (bvadd g (sha256-state-g s))
   (bvadd h (sha256-state-h s))))

(define (sha256-finalize s)
  (concat
   (sha256-state-a s)
   (sha256-state-b s)
   (sha256-state-c s)
   (sha256-state-d s)
   (sha256-state-e s)
   (sha256-state-f s)
   (sha256-state-g s)
   (sha256-state-h s)))

(define (message->blocks M)
  (define w (bitvector-size (type-of M)))
  (let loop ([acc '()]
             [i 511])
    (if (i . > . w)
        acc
        (loop (cons (extract i (- i 511) M) acc)
              (+ i 512)))))

(define (sha256-padded-blocks blocks)
  (let loop ([s (sha256-init)]
             [b blocks])
    (if (empty? b)
        (sha256-finalize s)
        (loop (sha256-process-block s (car b))
              (cdr b)))))

(define (sha256 m)
  (define M (pad-message m))
  (define blocks (message->blocks M))
  (sha256-padded-blocks blocks))

(define (string->bits str)
  (apply
   concat
   (map
    (lambda (c)
      (bv (!char->integer c) 8))
    (!string->list str))))

(define (sha256-string str)
  (sha256 (string->bits str)))

(module+ test
  (require rackunit)

  (test-case "small"
    (check-equal?
     (sha256-string "abc")
      (bv #xba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad 256)))
  
  (test-case "multi block"
    (check-equal?
     (sha256-string "asdoifjasdjiasifjasdfsdg908fdaugdsfug030854tj0dsafg0dsiuf9234utuaj0dfjudsf9g9sfdgu9sdfu834ut009fsd")
     (bv #xffcdaf3f44439fabd8a10cf81b691161209810f728cedcb0699e38f6fbcbf54e 256))))
