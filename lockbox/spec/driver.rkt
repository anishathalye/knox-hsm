#lang knox/driver

#:idle [en #f]

(define (store secret password)
  (out* 'en #t 'op #t 'secret secret 'password password)
  (tick)
  #t)

(define (get guess)
  (out* 'en #t 'op #f 'password guess)
  (tick)
  (let ([ret (output-out (in))])
    ;; allow returned_secret to become 0 again
    (out* 'en #f)
    (tick)
    ret))
