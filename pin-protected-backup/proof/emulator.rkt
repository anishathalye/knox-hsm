#lang knox/emulator

(struct state (circuit))

(define (initial-circuit)
  (circuit-with-input
   (circuit-step
    (circuit-with-input
     (circuit-new)
     (input* 'resetn #f 'cts #t 'rx #t)))
   (input* 'resetn #t 'cts #t 'rx #t)))

(define (init)
  (set! (state (initial-circuit))))

(define (with-input i)
  (set! (state (circuit-with-input (state-circuit (get)) i))))

(define (get-output)
  (circuit-get-output (state-circuit (get))))

(define (swap32 b)
  (concat
   (extract 7 0 b)
   (extract 15 8 b)
   (extract 23 16 b)
   (extract 31 24 b)))

(define (step)
  ;; initialization
  (let ([c (state-circuit (get))])
    (if (and (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2)) (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #x4 32)) (equal? (get-field c 'wrapper.soc.cpu.cpu_state) (bv #x20 8)))
        ;; do this early at the start
        (let ([fram (get-field c 'wrapper.soc.fram.fram)])
          (let ([leakage (spec:leak)])
            (displayln "status triggered")
            (set! (state (update-field c 'wrapper.soc.fram.fram
                                       (vector-set
                                        (vector-set
                                         (vector-set
                                          (vector-set
                                           fram
                                           0
                                           (concat (bv 0 16) (list-ref leakage 0) (if (spec:status (bv 0 8)) (bv 1 8) (bv 0 8))))
                                          6
                                          (concat (bv 0 16) (list-ref leakage 1) (if (spec:status (bv 1 8)) (bv 1 8) (bv 0 8))))
                                         12
                                         (concat (bv 0 16) (list-ref leakage 2) (if (spec:status (bv 2 8)) (bv 1 8) (bv 0 8))))
                                        18
                                        (concat (bv 0 16) (list-ref leakage 3) (if (spec:status (bv 3 8)) (bv 1 8) (bv 0 8)))))))))
        (void)))

  ;; store
  (let ([c (state-circuit (get))])
    (if (and
         (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2))
         (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #x1c8 32))
         (equal? (get-field c 'wrapper.soc.cpu.cpu_state) (bv #x02 8))
         (equal? (get-field c 'wrapper.soc.cpu.mem_state) (bv #b10 2)))
        (let ([a3 (vector-ref (get-field c 'wrapper.soc.cpu.cpuregs) 13)])
          (let ([offset-words (quotient (bitvector->natural (bvsub a3 (bv #x10000000 32))) 4)])
            (let ([slot (quotient offset-words 6)])
              (let ([fram (get-field c 'wrapper.soc.fram.fram)])
                (displayln "store triggered")
                (spec:store
                 (bv slot 8)
                 (swap32 (vector-ref fram (add1 offset-words)))
                 (concat
                  (swap32 (vector-ref fram (+ offset-words 2)))
                  (swap32 (vector-ref fram (+ offset-words 3)))
                  (swap32 (vector-ref fram (+ offset-words 4)))
                  (swap32 (vector-ref fram (+ offset-words 5)))))))))
        (void)))

  ;; delete
  (let ([c (state-circuit (get))])
    (if (and
         (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2))
         (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #xfc 32))
         (equal? (get-field c 'wrapper.soc.cpu.cpu_state) (bv #x02 8))
         (equal? (get-field c 'wrapper.soc.cpu.mem_state) (bv #b10 2)))
        (let ([a5 (vector-ref (get-field c 'wrapper.soc.cpu.cpuregs) 15)])
          (let ([offset-words (quotient (bitvector->natural (bvsub a5 (bv #x10000000 32))) 4)])
            (let ([slot (quotient offset-words 6)])
              (displayln "delete triggered")
              (spec:delete (bv slot 8)))))
        (void)))

  ;; retrieve
  (let ([c (state-circuit (get))])
    (if (and
         (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2))
         (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #x29c 32))
         (equal? (get-field c 'wrapper.soc.cpu.cpu_state) (bv #x02 8))
         (equal? (get-field c 'wrapper.soc.cpu.mem_state) (bv #b10 2)))
        (let ([s1 (vector-ref (get-field c 'wrapper.soc.cpu.cpuregs) 9)])
          (let ([offset-words (quotient (bitvector->natural (bvsub s1 (bv #x10000000 32))) 4)])
            (let ([slot (quotient offset-words 6)]
                  [pin (vector-ref (get-field c 'wrapper.soc.ram.ram) 499)])
              (displayln "retrieve triggered")
              (let ([maybe-secret (spec:retrieve (bv slot 8) (swap32 pin))])
                ;; inject equal flag + secret back into circuit so it produces right output
                (if maybe-secret
                    (set! (state (update-fields c
                                                (list (cons 'wrapper.soc.cpu.cpuregs (vector-set (get-field c 'wrapper.soc.cpu.cpuregs) 13 (bv 1 32)))
                                                      (cons 'wrapper.soc.fram.fram (write-secret (get-field c 'wrapper.soc.fram.fram) offset-words maybe-secret))))))
                    (set! (state (update-field c 'wrapper.soc.cpu.cpuregs
                                               (vector-set (get-field c 'wrapper.soc.cpu.cpuregs) 13 (bv 0 32))))))))))
        (void)))

  ;; zero out fram at poweroff
  (let ([c (state-circuit (get))])
    (if (and
         (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2))
         (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #xac 32)))
        (set! (state (update-field c 'wrapper.soc.fram.fram (get-field (circuit-zeroed) 'wrapper.soc.fram.fram))))
        (void)))

  (set! (state (circuit-step (state-circuit (get))))))

(define (write-secret fram slot-offset-words secret)
  (vector-set
   (vector-set
    (vector-set
     (vector-set
      fram
      (+ slot-offset-words 2)
      (swap32 (extract 127 96 secret)))
     (+ slot-offset-words 3)
     (swap32 (extract 95 64 secret)))
    (+ slot-offset-words 4)
    (swap32 (extract 63 32 secret)))
   (+ slot-offset-words 5)
   (swap32 (extract 31 0 secret))))
