#lang knox/spec

#:init s0
#:symbolic-constructor new-symbolic-state
#:methods
(store [tag (bitvector TAG-WIDTH)] [secret (bitvector WIDTH)] [password (bitvector WIDTH)])
(get [tag (bitvector TAG-WIDTH)] [guess (bitvector WIDTH)])

(require rosutil)

(provide WIDTH TAG-WIDTH (struct-out row) (struct-out state) new-symbolic-state new-zeroed-state store get row-empty)

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
