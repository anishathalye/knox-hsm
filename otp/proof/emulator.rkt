#lang knox/emulator

(struct state (circuit otp-value))

(define (initial-circuit)
  (circuit-with-input
   (circuit-step
    (circuit-with-input
     (circuit-new)
     (input* 'resetn #f 'cts #t 'rx #t)))
   (input* 'resetn #t 'cts #t 'rx #t)))

(define (init)
  (set! (state (initial-circuit) (bv 0 32))))

(define (with-input i)
  (set! (state (circuit-with-input (state-circuit (get)) i)
               (state-otp-value (get)))))

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
  (let ([c (state-circuit (get))])
    (if (and
         (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2))
         (or
          (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #x810 32)) ; first instruction in do_get_otp
          (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #x778 32))) ; first instruction in audit
         (equal? (get-field c 'wrapper.soc.cpu.cpu_state) (bv #x20 8)))
        (let ([fram (get-field c 'wrapper.soc.fram.fram)]
              [ctr (spec:audit)])
          ;; simulator has fram initially hard-coded to 0, so active is 0, so secret is written into 1
          (displayln "audit triggered")
          (set! (state (update-field c 'wrapper.soc.fram.fram
                                     (vector-set* fram (list 8 (extract 31 0 ctr)
                                                             9 (extract 63 32 ctr))))
                       (bv 0 32)))) ; this is ok, we haven't computed OTP yet
        (void)))
  ;; compute hash: need to do this at commit point of write (new counter value)
  ;; but inject it later
  (let ([c (state-circuit (get))])
    (if (and
         (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2))
         (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #x910 32)) ; at commit point
         (equal? (get-field c 'wrapper.soc.cpu.cpu_state) (bv #x02 8))
         (equal? (get-field c 'wrapper.soc.cpu.mem_state) (bv #b10 2)))
        (let ([ram (get-field c 'wrapper.soc.ram.ram)])
          ;; compute hash to update spec max-ctr
          (let ([h (spec:otp (concat (swap32 (vector-ref ram 497))
                                     (swap32 (vector-ref ram 498))))])
            (set! (state c h))))
        (void)))
  ;; zero out argument `c` to hotp, so HMAC computation is
  ;; deterministic (no need to create lots of symbolics...)
  (let ([c (state-circuit (get))])
    (if (and
         (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2))
         (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #x510 32)) ; right at the start of hotp()
         (equal? (get-field c 'wrapper.soc.cpu.cpu_state) (bv #x20 8)))
        (let ([ram (get-field c 'wrapper.soc.ram.ram)])
          (set! (state (update-field c 'wrapper.soc.ram.ram
                                     (vector-set* ram (list 497 (bv 0 32)
                                                            498 (bv 0 32))))
                       (state-otp-value (get)))))
        (void)))
  ;; inject HOTP value into s0 register, after the `mv s0,a0` instruction
  (let ([c (state-circuit (get))])
    (if (and
         (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2))
         (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #x948 32)) ; right after mv s0,a0
         (equal? (get-field c 'wrapper.soc.cpu.cpu_state) (bv #x20 8)))
        (let ([cpuregs (get-field c 'wrapper.soc.cpu.cpuregs)])
          (set! (state (update-field c 'wrapper.soc.cpu.cpuregs
                                     (vector-set cpuregs 8 (state-otp-value (get))))
                       (bv 0 32))))
        (void)))
  ;; set-secret commit point
  (let ([c (state-circuit (get))])
    (if (and
         (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2))
         (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #x764 32)) ; at commit point
         (equal? (get-field c 'wrapper.soc.cpu.cpu_state) (bv #x02 8))
         (equal? (get-field c 'wrapper.soc.cpu.mem_state) (bv #b10 2)))
        (let ([ram (get-field c 'wrapper.soc.ram.ram)])
          (spec:set-secret
           (concat (swap32 (vector-ref ram 499))
                   (swap32 (vector-ref ram 500))
                   (swap32 (vector-ref ram 501))
                   (swap32 (vector-ref ram 502))
                   (swap32 (vector-ref ram 503)))))
        (void)))
  ;; zero out fram at poweroff
  (let ([c (state-circuit (get))])
    (if (and
         (equal? (get-field c 'wrapper.pwrmgr_state) (bv #b10 2))
         (equal? (get-field c 'wrapper.soc.cpu.reg_pc) (bv #xb0 32)))
        (set! (state (update-field c 'wrapper.soc.fram.fram (get-field (circuit-zeroed) 'wrapper.soc.fram.fram))
                     (bv 0 32))) ; also forget OTP
        (void)))

  (set! (state (circuit-step (state-circuit (get)))
               (state-otp-value (get)))))
