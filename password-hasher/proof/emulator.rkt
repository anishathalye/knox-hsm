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

(define (vector-set* v l)
  (if (empty? l)
      v
      (vector-set* (vector-set v (first l) (second l)) (cddr l))))

(define (step)
  ;; store
  (let ([c (state-circuit (get))])
    (if (and
         (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2))
         (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #x448 32))
         (equal? (get-field c 'wrapper.soc.cpu.cpu_state) (bv #x02 8))
         (equal? (get-field c 'wrapper.soc.cpu.mem_state) (bv #b10 2)))
        (let ([fram (get-field c 'wrapper.soc.fram.fram)])
          ;; emulator has fram initially hard-coded to 0, so active is 0, so secret is written into 1
          (displayln "store triggered")
          (spec:set-secret
           (concat
            (swap32 (vector-ref fram 6))
            (swap32 (vector-ref fram 7))
            (swap32 (vector-ref fram 8))
            (swap32 (vector-ref fram 9))
            (swap32 (vector-ref fram 10)))))
        (void)))
  ;; hash, doesn't matter exactly where we do this, as long as it's after op and before uart writes
  (let ([c (state-circuit (get))])
    (if (and
         (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2))
         (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #x4ec 32)) ; right after return from sha256_digest
         (equal? (get-field c 'wrapper.soc.cpu.cpu_state) (bv #x20 8)))
        (let ([ram (get-field c 'wrapper.soc.ram.ram)])
          ;; compute hash
          (let ([h (spec:get-hash (concat (swap32 (vector-ref ram 492))
                                          (swap32 (vector-ref ram 493))
                                          (swap32 (vector-ref ram 494))
                                          (swap32 (vector-ref ram 495))
                                          (swap32 (vector-ref ram 496))
                                          (swap32 (vector-ref ram 497))
                                          (swap32 (vector-ref ram 498))
                                          (swap32 (vector-ref ram 499))))])
            ;; inject into emulated circuit
            (set! (state (update-field c 'wrapper.soc.ram.ram
                                       (vector-set* ram (list 513 (swap32 (extract 255 224 h))
                                                              514 (swap32 (extract 223 192 h))
                                                              515 (swap32 (extract 191 160 h))
                                                              516 (swap32 (extract 159 128 h))
                                                              517 (swap32 (extract 127 96 h))
                                                              518 (swap32 (extract 95 64 h))
                                                              519 (swap32 (extract 63 32 h))
                                                              520 (swap32 (extract 31 0 h)))))))))
        (void)))
  ;; zero out fram at poweroff
  (let ([c (state-circuit (get))])
    (if (and
         (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2))
         (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #xac 32)))
        (set! (state (update-field c 'wrapper.soc.fram.fram (get-field (circuit-zeroed) 'wrapper.soc.fram.fram))))
        (void)))

  (set! (state (circuit-step (state-circuit (get))))))
