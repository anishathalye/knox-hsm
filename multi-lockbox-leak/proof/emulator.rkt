#lang knox/emulator

(struct state (circuit cached-output))

(define WIDTH 128)

(define (boolean->bitvector b)
  (if b (bv 1 1) (bv 0 1)))

(define (init)
  (let ([l (spec:leak)])
    (let ([valid0 (car l)]
          [tag0 (car (cdr l))]
          [valid1 (car (cdr (cdr l)))]
          [tag1 (car (cdr (cdr (cdr l))))])
      (let ([c (update-fields
                (circuit-new)
                (list
                 (cons 'row_valid (vector-immutable (boolean->bitvector valid0) (boolean->bitvector valid1)))
                 (cons 'tags (vector-immutable tag0 tag1))))])
        (set! (state c #f))))))

(define (with-input i)
  (set! (state (circuit-with-input (state-circuit (get)) i)
               (state-cached-output (get)))))

(define (get-output)
  ;; if the circuit is giving "real" output, serve cached output from call into oracle
  (let ([c (state-circuit (get))])
    (if (output-o_valid (circuit-get-output c))
        (output* 'o_out (state-cached-output (get)) 'o_valid #t)
        (circuit-get-output c))))

(define (step)
  ;; step, and cache output if necessary
  (let ([c1 (state-circuit (get))])
    (let ([c2 (circuit-step c1)])
      (set!
       (state
        c2
        (if (output-o_valid (circuit-get-output c2))
            ;; call into oracle, cache output
            (let ([op (get-field c2 'saved_op)]
                  [tag (get-field c2 'saved_tag)]
                  [password (get-field c2 'saved_password)]
                  [secret (get-field c2 'saved_secret)])
              (if (bveq op (bv 0 1))
                  ;; get
                  (spec:get tag password)
                  ;; store
                  (if (spec:store tag secret password) (bv 1 WIDTH) (bv 0 WIDTH))))
            ;; don't care
            #f))))))
