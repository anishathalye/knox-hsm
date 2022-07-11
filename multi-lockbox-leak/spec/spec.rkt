#lang knox/spec

#:init s0
#:symbolic-constructor new-symbolic-state
#:methods
(store [tag (bitvector TAG-WIDTH)] [secret (bitvector WIDTH)] [password (bitvector WIDTH)])
(get [tag (bitvector TAG-WIDTH)] [guess (bitvector WIDTH)])
#:leak leak

(require rosutil)

(provide WIDTH TAG-WIDTH (struct-out row) (struct-out state) new-symbolic-state new-zeroed-state store get leak)

(define WIDTH 128)
(define TAG-WIDTH 16)

(struct row (valid tag secret password))

(define (new-symbolic-row)
  (row
   (fresh-symbolic 'valid boolean?)
   (fresh-symbolic 'tag (bitvector TAG-WIDTH))
   (fresh-symbolic 'secret (bitvector WIDTH))
   (fresh-symbolic 'password (bitvector WIDTH))))

(define row-empty
  (row #f (bv 0 TAG-WIDTH) (bv 0 WIDTH) (bv 0 WIDTH)))

(struct state (row0 row1))

(define s0 (state row-empty row-empty))

(define (new-symbolic-state)
  (state
   (new-symbolic-row)
   (new-symbolic-row)))

(define (new-zeroed-state)
  (state row-empty row-empty))

(define ((store tag secret password) s)
  (define row0 (state-row0 s))
  (define row1 (state-row1 s))
  (define new-row (row #t tag secret password))
  (cond
    ;; does tag match any valid row? if so, overwrite
    [(and (row-valid row0) (equal? (row-tag row0) tag))
     (result #t (state new-row row1))]
    [(and (row-valid row1) (equal? (row-tag row1) tag))
     (result #t (state row0 new-row))]
    ;; are there any unused rows?
    [(not (row-valid row0))
     (result #t (state new-row row1))]
    [(not (row-valid row1))
     (result #t (state row0 new-row))]
    ;; no space to store
    [else (result #f s)]))

(define ((get tag guess) s)
  (define row0 (state-row0 s))
  (define row1 (state-row1 s))
  (cond
    [(and (row-valid row0) (equal? (row-tag row0) tag))
     (result
      (if (equal? (row-password row0) guess) (row-secret row0) (bv 0 WIDTH))
      (state row-empty row1))]
    [(and (row-valid row1) (equal? (row-tag row1) tag))
     (result
      (if (equal? (row-password row1) guess) (row-secret row1) (bv 0 WIDTH))
      (state row0 row-empty))]
    [else (result (bv 0 WIDTH) s)]))

(define ((leak) s)
  (define row0 (state-row0 s))
  (define row1 (state-row1 s))
  (result
   (list
    (row-valid row0)
    (if (row-valid row0) (row-tag row0) (bv 0 TAG-WIDTH))
    (row-valid row1)
    (if (row-valid row1) (row-tag row1) (bv 0 TAG-WIDTH)))
   s))

(module+ test
  (require rackunit)

  (test-case "basic"
    (define s0 (state row-empty row-empty))
    (define r1 ((store (bv 34 TAG-WIDTH) (bv 1337 WIDTH) (bv 1234 WIDTH)) s0))
    (define s1 (result-state r1))
    (check-equal? (result-value r1) #t)
    ;; wrong password guess
    (define bad2 ((get (bv 34 TAG-WIDTH) (bv 0 WIDTH)) s1))
    (check-equal? (result-value bad2) (bv 0 WIDTH))
    ;; subsequent correct guess doesn't do anything useful
    (check-equal? (result-value ((get (bv 34 TAG-WIDTH) (bv 1234 WIDTH)) (result-state bad2))) (bv 0 WIDTH))
    ;; correct guess initially gets back the secret
    (check-equal? (result-value ((get (bv 34 TAG-WIDTH) (bv 1234 WIDTH)) s1)) (bv 1337 WIDTH))))
