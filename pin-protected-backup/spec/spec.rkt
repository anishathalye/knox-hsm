#lang knox/spec

#:init s0
#:symbolic-constructor new-symbolic-state
#:methods
(status [slot (bitvector 8)])
(delete [slot (bitvector 8)])
(store [slot (bitvector 8)] [pin (bitvector PIN-LENGTH)] [data (bitvector DATA-SIZE)])
(retrieve [slot (bitvector 8)] [pin (bitvector PIN-LENGTH)])
#:leak leak

(require rosutil
         (only-in racket/base build-list))

(provide
 PIN-LENGTH
 PIN-BYTES
 DATA-SIZE
 DATA-BYTES
 NENTRY
 GUESS-LIMIT
 (struct-out entry)
 new-symbolic-state
 s0
 e0
 status delete store retrieve leak)

(define PIN-LENGTH (* 8 4))
(define PIN-BYTES 4)
(define DATA-SIZE (* 8 16))
(define DATA-BYTES 16)
(define NENTRY 4)
(define GUESS-LIMIT 10)

(struct entry (valid bad-guesses pin data))

(define (new-symbolic-entry)
  (define valid (fresh-symbolic 'valid boolean?))
  (if valid
      (entry
       #t
       (fresh-symbolic 'bad-guesses (bitvector 8))
       (fresh-symbolic 'pin (bitvector PIN-LENGTH))
       (fresh-symbolic 'data (bitvector DATA-SIZE)))
      e0))

(define e0 (entry #f (bv 0 8) (bv 0 PIN-LENGTH) (bv 0 DATA-SIZE)))

;; a state is a list of NENTRY entries

(define (new-symbolic-state)
  (build-list NENTRY (lambda _ (new-symbolic-entry))))

(define s0 (build-list NENTRY (lambda _ e0)))

(define ((status slot) s)
  (if (bvult slot (bv NENTRY 8))
      (result (entry-valid (list-ref-bv s slot)) s)
      (result #f s)))

(define ((delete slot) s)
  (if (bvult slot (bv NENTRY 8))
      (let ()
        (define was-valid (entry-valid (list-ref-bv s slot)))
        (result was-valid (list-set-bv s slot e0)))
      (result #f s)))

(define ((store slot pin data) s)
  (cond
    [(bvuge slot (bv NENTRY 8)) (result #f s)]
    [(entry-valid (list-ref-bv s slot)) (result #f s)]
    [else
     (result #t (list-set-bv s slot (entry #t (bv 0 8) pin data)))]))

(define ((retrieve slot pin) s)
  (if (bvult slot (bv NENTRY 8))
      (let ([e (list-ref-bv s slot)])
        (cond
          [(and (entry-valid e)
                (bvult (entry-bad-guesses e) (bv GUESS-LIMIT 8)))
           (cond
             [(equal? (entry-pin e) pin)
              (result (entry-data e) (list-set-bv s slot (entry #t (bv 0 8) (entry-pin e) (entry-data e))))]
             [else
              (result #f (list-set-bv s slot (entry #t (bvadd (entry-bad-guesses e) (bv 1 8)) (entry-pin e) (entry-data e))))])]
          [else
           (result #f s)]))
      (result #f s)))

;; we leak guess counts
(define ((leak) s)
  (result (map entry-bad-guesses s) s))

(module+ test
  (require rackunit)

  (test-case "basic"
    (check-equal? (result-value ((status (bv 0 8)) s0)) #f)
    (define s1 (result-state ((store (bv 3 8) (bv 1234 PIN-LENGTH) (bv 1337 DATA-SIZE)) s0)))
    ;; status
    (check-equal? (result-value ((status (bv 3 8)) s1)) #t)
    ;; correct guess
    (check-equal? (result-value ((retrieve (bv 3 8) (bv 1234 PIN-LENGTH)) s1)) (bv 1337 DATA-SIZE))
    ;; bad guess
    (check-equal? (result-value ((retrieve (bv 3 8) (bv 1111 PIN-LENGTH)) s1)) #f)
    ;; one bad guess is okay
    (check-equal? (result-value ((retrieve (bv 3 8) (bv 1234 PIN-LENGTH))
                                 (result-state ((retrieve (bv 3 8) (bv 1111 PIN-LENGTH)) s1))))
                  (bv 1337 DATA-SIZE))
    ;; deletion
    (define del2 (result-state ((delete (bv 3 8)) s1)))
    (check-equal? (result-value ((retrieve (bv 3 8) (bv 1234 PIN-LENGTH)) del2)) #f)
    ;; guess limit enforcement
    (define guess2
      (let loop ([s s1]
                 [n GUESS-LIMIT])
        (if (zero? n) s (loop (result-state ((retrieve (bv 3 8) (bv 1111 PIN-LENGTH)) s)) (sub1 n)))))
    (check-equal? (result-value ((retrieve (bv 3 8) (bv 1234 PIN-LENGTH)) guess2)) #f)))
