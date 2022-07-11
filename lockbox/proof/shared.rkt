#lang rosette/safe

(require rosutil)

(provide R)

(define (R f c)
  (and
   ;; invariant: returned_secret is 0 in between operations
   (equal? (get-field c 'returned_secret) (bv 0 128))
   (equal? (get-field f 'secret)
           (get-field c 'stored_secret))
   (if (equal? (get-field f 'secret) (bv 0 128))
       #t
       (equal? (get-field f 'password)
               (get-field c 'stored_password)))))
