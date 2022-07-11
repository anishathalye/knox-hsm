#lang knox/driver

#:idle [i_en #f]

(define WIDTH 128)

(define (wait-until-valid)
  (hint concretize)
  (if (output-o_valid (in))
      (void)
      (begin
        (tick)
        (wait-until-valid))))

(define (store tag secret password)
  (reset)
  (out* 'i_en #t 'i_op #t 'i_tag tag 'i_secret secret 'i_password password)
  (tick)
  (out* 'i_en #f)
  (hint cases-store-tag-match)
  (wait-until-valid)
  (let ([res (output-o_out (in))])
    (tick)
    (not (bveq res (bv 0 WIDTH)))))

(define (get tag guess)
  (reset)
  (out* 'i_en #t 'i_op #f 'i_tag tag 'i_secret (bv 0 128) 'i_password guess)
  (tick)
  (out* 'i_en #f)
  (hint cases-get-tag-match)
  (wait-until-valid)
  (let ([res (output-o_out (in))])
    (tick)
    res))

(define (reset)
  (out* 'i_rst_n #f)
  (tick)
  (hint concretize)
  (out* 'i_rst_n #t))
