#lang knox/correctness

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:driver "../spec/driver.rkt"
#:R R
#:hints hints
;; #:only 'otp
;; #:without-crashes #t
;; #:without-yield #t
#:verbose #t

(require
 "shared.rkt"
 (prefix-in spec: "../spec/spec.rkt")
 (only-in "../spec/spec-sha1.rkt" pad-message message->blocks sha1-init sha1-finalize)
 (only-in "../spec/spec-hmac-sha1.rkt" hmac-sha1)
 (only-in "../spec/spec-hotp.rkt" hotp dt DIGITS)
 knox/correctness/hint
 knox/correctness/checker
 racket/match
 rosette/safe
 yosys/meta
 knox/spec
 knox/circuit
 (only-in rosette/base/core/polymorphic ite)
 (except-in rosutil concretize overapproximate))

(define (hints method c1 f1 f-out f2)
  (match method
    [`(audit)
     (extend-hintdb
      common-hintdb
      [overapproximate-boot-pc (overapproximate-pc (R f1 c1))]
      [maybe-split-branchless (split-branchless #x7a0 15)]
      [maybe-merge-after-recv (merge-at-pc #x9e0)])]
    [`(set-secret ,secret)
     (extend-hintdb
      common-hintdb
      [overapproximate-boot-pc (overapproximate-pc (R f1 c1))]
      [maybe-split-branchless (split-branchless #x710 12)]
      [maybe-merge-after-recv (merge-at-pc #x6fc)])]
    [`(otp ,ctr)
     (extend-hintdb
      common-hintdb
      [overapproximate-boot-pc (overapproximate-pc (R f1 c1))]
      [maybe-split-branchless (split-branchless #x85c 10)]
      [maybe-merge-after-recv (merge-at-pc #x83c)]
      [handle-otp
       (let* ([arg1 (message->blocks (pad-message (hmac-sha1-arg1 (spec:state-secret f1) ctr)))]
              [arg2 (void)] ; compute later
              [imp (imp-init (first arg1))]
              [stage 0]
              [previous-sha1-state (bv #b00 2)]
              [previous-vars #f]
              [triggered-return-mod-10^6 #f])
         (tactic
          (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
          (define sha1-state (get-field ckt 'wrapper.soc.sha1.state))
          (define state-00->01 (and (equal? previous-sha1-state (bv #b00 2)) (equal? sha1-state (bv #b01 2))))
          (define state-10->00 (and (equal? previous-sha1-state (bv #b10 2)) (equal? sha1-state (bv #b00 2))))
          (define cpu-pc (get-field ckt 'wrapper.soc.cpu.reg_pc))
          (define cpu-state (get-field ckt 'wrapper.soc.cpu.cpu_state))
          (set! previous-sha1-state sha1-state)
          (when (or (and (not state-00->01) (not (equal? sha1-state (bv #b00 2))))
                    state-10->00)
            (eprintf "sha1: stepping imp~n")
            (set! imp (imp-step imp)))
            ;; inject, subst, abstract
            ;; and on state-10->00, set imp to next thing
            (cond
              [(or (not (equal? sha1-state (bv #b00 2))) state-10->00)
               (eprintf "sha1: inject, subst, clear, abstract~n")
               ;; inject
               (for ([el impl-spec-lenses])
                 (replace (lens 'circuit (second el)) (lens-view (third el) imp)))
               (subst (lens 'circuit (list #rx"sha1\\.([a-e]|h[0-4])" (lens 'wrapper.soc.sha1.w_mem vector-all-elements-lens))))
               (clear (if previous-vars previous-vars (list)))
               (set! previous-vars #f) ; used up
               ;; abstract, keep track of vars
               (set! previous-vars '())
               (for ([el impl-spec-lenses])
                 (define var (remember (lens 'circuit (second el)) (first el)))
                 (set! imp (lens-set (third el) imp var))
                 (set! previous-vars (cons var previous-vars)))
               (when state-10->00
                 (cond
                   [(equal? stage 0)
                    (set! imp (imp-next imp (second arg1)))]
                   [(equal? stage 1)
                    (set! arg2 (message->blocks (pad-message (hmac-sha1-arg2 imp (spec:state-secret f1)))))
                    (set! imp (imp-init (first arg2)))]
                   [(equal? stage 2)
                    (set! imp (imp-next imp (second arg2)))])
                 (eprintf "imp next, stage: ~v, symbolics in new imp: ~v~n"
                          stage
                          (symbolics imp))
                 (set! stage (add1 stage)))]
              [(and (equal? cpu-pc (bv #x5a4 32)) (equal? cpu-state (bv #x40 8)))
               ;; just before call to constant time mod 10^6
               (eprintf "just before call to constant time mod 10^6~n")
               (define hmac-output (imp-finalize imp))
               (define s (dt hmac-output))
               (replace (lens 'circuit 'wrapper.soc.cpu.cpuregs 10) s)]
              [(and (equal? cpu-pc (bv #x944 32)) (not triggered-return-mod-10^6))
               (set! triggered-return-mod-10^6 #t) ; only once
               (eprintf "returned from constant time mod 10^6~n")
               ;; returned from mod 10^6
               (define hmac-output (imp-finalize imp))
               (define s (dt hmac-output))
               (define spec-otp (bvurem s (bv (expt 10 6) 32)))
               (replace (lens 'circuit 'wrapper.soc.cpu.cpuregs 10) spec-otp)
               (subst (lens 'circuit 'wrapper.soc.cpu.cpuregs 10))
               (clear)]
              [(and (equal? cpu-pc (bv #x94c 32)) (equal? cpu-state (bv #x20 8)))
               (eprintf "returned from hotp")
               (remember (lens 'circuit 'wrapper.soc.cpu.cpuregs 8) 'hotp)])))]
      [finalize
       (tactic
        ;; remember that finalize is called regardless of
        ;; whether OTP returns false; need to handle both cases here
        (define eq (hash-keys (checker-state-equalities (get-state))))
        (define value-lens (lens 'interpreter 'environment (list-ref-lens 0) cdr-lens))
        (cond
          [(not (empty? eq)) ; case where we computed the HOTP
           ;; first, clean up the output we got from the UART; it's just the 32-bit value hotp,
           ;; but it's been mangled by going through the UART. there's only one equality in
           ;; the set of equalities, so grab it from there
           (define hotp (first eq))
           (replace value-lens hotp)
           ;; spec has this ite in it, though in this branch, that first case is false
           ;; but the following makes it so that the circuit return
           ;; value is rosette/equal? to the spec return value
           ;;
           ;; this is somewhat slow, though probably not worth figuring out how to optimize
           (define fully-aligned-with-spec (if (bvult ctr (spec:state-max-ctr f1)) (bv 0 32) (lens-view value-lens (get-state))))
           (replace value-lens fully-aligned-with-spec #:use-pc #t)
           (subst value-lens #f)]
          [else
           (replace value-lens f-out #:use-pc #t)]))]
      [maybe-replace-and-merge-after-cases
       ;; concretizing
       (tactic
        (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
        (when (and (equal? (get-field ckt 'wrapper.soc.cpu.reg_pc) (bv #x940 32)))
          ;; replace secret that is in buf
          (eprintf "pc: ~v state: ~v, rewriting secret and merging~n" (get-field ckt 'wrapper.soc.cpu.reg_pc) (get-field ckt 'wrapper.soc.cpu.cpu_state))
          (define path (checker-state-pc (get-state)))
          (eprintf "path condition: ~v~n" path)
          (define f1-secret (spec:state-secret f1))
          (eprintf "about to replace, ram[499] = ~v, f1-secret[159:128] = ~v~npc: ~v~n"
                   (lens-view (lens 'wrapper.soc.ram.ram 499) ckt)
                   (swap32 (extract 159 128 f1-secret))
                   (checker-state-pc (get-state)))
          (replace* (list (cons (lens 'circuit 'wrapper.soc.ram.ram 499) (swap32 (extract 159 128 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.ram.ram 500) (swap32 (extract 127 96 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.ram.ram 501) (swap32 (extract 95 64 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.ram.ram 502) (swap32 (extract 63 32 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.ram.ram 503) (swap32 (extract 31 0 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.fram.fram 2) (swap32 (extract 159 128 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.fram.fram 10) (swap32 (extract 159 128 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.fram.fram 3) (swap32 (extract 127 96 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.fram.fram 11) (swap32 (extract 127 96 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.fram.fram 4) (swap32 (extract 95 64 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.fram.fram 12) (swap32 (extract 95 64 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.fram.fram 5) (swap32 (extract 63 32 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.fram.fram 13) (swap32 (extract 63 32 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.fram.fram 6) (swap32 (extract 31 0 f1-secret)))
                          (cons (lens 'circuit 'wrapper.soc.fram.fram 14) (swap32 (extract 31 0 f1-secret))))
                    #:use-pc #t)
          (overapproximate (lens 'circuit (list 'wrapper.soc.cpu.mem_wdata
                                                'wrapper.soc.cpu.count_instr ; this is the same, but easier if we overapproximate it
                                                (lens 'wrapper.soc.cpu.cpuregs (list 6 13 14 15 16 17 28)))))
          ;; we need to remember a couple things here, including (R f1 c1), because otherwise we won't
          ;; be able to prove (R f1 c1); we haven't "overwritten" the secret in the fram,
          ;; we need to remember the relation (we can't just overwrite because we don't know which one it is)
          ;;
          ;; we also need to remember that ctr >= max-ctr
          ;;
          ;; note: the merge here can look confusing because there will be 8 cases (active=0/1) * (upper half of ctr >/<=) * (lower half of ctr >/<=)
          ;;
          ;; only 4 of these cases are "real", and we want to merge them
          ;;
          ;; same strategy as password hasher doesn't work here; this is a read-write operation, the FRAM state has actually changed,
          ;; we need to use techniques like in the security proof to line it up
          (overapproximate-pc (and (bvuge ctr (spec:state-max-ctr f1)) (R f1 c1)))
          (merge)))])]))

(define common-hintdb
  (make-hintdb
   [handle-otp done]
   [finalize done]
   [maybe-replace-and-merge-after-cases done]
   [concretize-init (concretize (lens 'circuit (field-filter/or "pwrmgr_state" "resetn" "rom")) #:use-pc #t)]
   [concretize-uart
    (concretize (lens 'circuit (field-filter/or 'wrapper.soc.uart.simpleuart.recv_buf_data 'wrapper.soc.uart.simpleuart.recv_pattern)))]
   [maybe-split-pc
    (tactic
     (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
     (define pc (get-field ckt 'wrapper.soc.cpu.reg_pc))
     (match pc
       [(expression (== ite) c _ _)
        (eprintf "case splitting on ~v~n" c)
        (case-split (list (! c) c))
        (concretize (lens 'circuit "wrapper.soc.cpu.") #:piecewise #t #:use-pc #t)]
       [_ (void)]))]
   [maybe-concretize-cpu
    (tactic
     (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
     (when (symbolic? (get-field ckt 'wrapper.soc.cpu.decoder_trigger))
       (concretize (lens 'circuit (field-filter/or 'wrapper.soc.cpu.decoder_trigger 'wrapper.soc.cpu.latched_branch
                                                   'wrapper.soc.cpu.latched_store 'wrapper.soc.cpu.mem_do_rinst))
                   #:use-pc #t)))]
   [uart-fp (fixpoint 56 #t 9
                      (lens (list 'wrapper.soc.cpu.reg_pc 'wrapper.soc.cpu.decoder_trigger 'wrapper.soc.cpu.latched_branch
                                  'wrapper.soc.cpu.latched_store 'wrapper.soc.cpu.mem_do_rinst))
                      (lens (list 'wrapper.soc.cpu.count_cycle 'wrapper.soc.cpu.count_instr
                                  'wrapper.soc.uart.simpleuart.send_divcnt)))]
   [overapproximate-uart (overapproximate (lens 'circuit (field-filter/or "send_divcnt")))]
   [overapproximate-boot (overapproximate (lens 'circuit (field-filter/or "count_cycle" "count_instr" "send_divcnt")))]
   [overapproximate-send-merge (overapproximate (lens 'circuit (field-filter/or "count_cycle" "count_instr" "send_divcnt")))]
   [overapproximate-recv-merge (overapproximate (lens 'circuit (field-filter/or "count_cycle" "count_instr")))]
   [merge (merge (lambda (s)
                   (list (get-field s 'wrapper.soc.cpu.reg_pc)
                         (vector-ref (get-field s 'wrapper.soc.cpu.cpuregs) 15)
                         (get-field s 'wrapper.soc.cpu.cpu_state)
                         (get-field s 'wrapper.soc.cpu.alu_out_q)
                         (get-field s 'wrapper.soc.cpu.reg_out)
                         (get-field s 'wrapper.soc.cpu.reg_sh)
                         (get-field s 'wrapper.soc.cpu.reg_op1)
                         (get-field s 'wrapper.soc.cpu.mem_state)
                         (get-field s 'wrapper.soc.cpu.mem_addr)
                         (get-field s 'wrapper.soc.uart.simpleuart.recv_buf_valid))))]
   [debug (tactic
           (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
           (eprintf "pc: ~v~n" (get-field ckt 'wrapper.soc.cpu.reg_pc)))]))

(define (replace* lens-value #:use-pc [use-pc #f])
  (for ([lvp (in-list lens-value)])
    (replace (car lvp) (cdr lvp) #:use-pc use-pc)))

(define (split-branchless pc reg)
  (tactic
   (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
   (when (and (equal? (get-field ckt 'wrapper.soc.cpu.reg_pc) (bv pc 32)) (equal? (get-field ckt 'wrapper.soc.cpu.cpu_state) (bv #x20 8)))
     (let ([ax (lens-view (lens 'wrapper.soc.cpu.cpuregs reg) ckt)])
       (displayln "case split on active = 0 or active = 1")
       (case-split (map (lambda (v) (equal? ax v)) (list (bv 0 32) (bv 1 32))))
       (concretize (lens 'circuit 'wrapper.soc.cpu.cpuregs reg) #:use-pc #t)))))

(define (merge-at-pc pc)
  (tactic
   (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
   (when (and (equal? (get-field ckt 'wrapper.soc.cpu.reg_pc) (bv pc 32)) (equal? (get-field ckt 'wrapper.soc.cpu.cpu_state) (bv #x20 8)))
     (let ()
       (eprintf "pc: ~v state: ~v, merging~n" (get-field ckt 'wrapper.soc.cpu.reg_pc) (get-field ckt 'wrapper.soc.cpu.cpu_state))
       (merge)))))
