#lang knox/spec

#:init s0
#:symbolic-constructor new-symbolic-state
#:methods
(set-secret [secret (bitvector SECRET-SIZE)])
(otp [ctr (bitvector CTR-SIZE)])
(audit)

(require rosutil
         "spec-hotp.rkt")

(provide
 SECRET-SIZE
 SECRET-SIZE-BYTES
 CTR-SIZE
 CTR-SIZE-BYTES
 (struct-out state)
 new-symbolic-state
 set-secret otp audit
 s0)

(struct state (secret max-ctr))

(define (new-symbolic-state)
  (state
   (fresh-symbolic 'secret (bitvector SECRET-SIZE))
   (fresh-symbolic 'max-ctr (bitvector CTR-SIZE))))

(define ((set-secret secret) s)
  (result #t (state secret (state-max-ctr s))))

(define ((audit) s)
  (result (state-max-ctr s) s))

(define ((otp ctr) s)
  (cond
    [(bvult ctr (state-max-ctr s))
     (result (bv 0 32) s)]
    [else (result (hotp (state-secret s) ctr)
                  (state (state-secret s) ctr))]))

(define s0 (state (bv 0 SECRET-SIZE) (bv 0 CTR-SIZE)))

(module+ test
  (require rackunit)

  (test-case "basic"
    (define s1 (result-state ((set-secret (bv #x1337 SECRET-SIZE)) s0)))
    (define r2 ((otp (bv 1234 CTR-SIZE)) s1))
    (check-equal? (result-value r2) (cons #t (bv 451349 32)))
    (define s2 (result-state r2))
    ;; counter can't be rewound
    (check-equal? (result-value ((otp (bv 1 CTR-SIZE)) s2)) (cons #f (bv 0 32)))
    ;; but can go forward
    (check-equal? (result-value ((otp (bv 9999 CTR-SIZE)) s2)) (cons #t (bv 910689 32)))
    ;; audit works
    (check-equal? (result-value ((audit) s2)) (bv 1234 CTR-SIZE))
    ;; changing secret does not reset bound
    (define s3 (result-state ((set-secret (bv #xcafe SECRET-SIZE)) s2)))
    (check-equal? (result-value ((otp (bv 1 CTR-SIZE)) s3)) (cons #f (bv 0 32)))))
