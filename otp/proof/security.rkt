#lang knox/security

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:emulator "emulator.rkt"
#:R R
;; #:skip-final-check #t

(require
 "shared.rkt"
 (only-in "../spec/spec-sha1.rkt" pad-message message->blocks sha1-init sha1-finalize)
 (only-in "../spec/spec-hmac-sha1.rkt" hmac-sha1)
 (only-in "../spec/spec-hotp.rkt" hotp dt DIGITS)
 (prefix-in spec: "../spec/spec.rkt")
 rosette/safe
 rosutil
 racket/match
 (prefix-in racket/ racket/base)
 (only-in racket/list range))

(define (cases*! preds)
  (cases! (append preds (list (not (apply || preds))))))

(define (show-pcs)
  (define t (set-term (current)))
  (define c (lens-view (lens 'circuit 'wrapper.soc.cpu.reg_pc) t))
  (define cs (lens-view (lens 'circuit 'wrapper.soc.cpu.cpu_state) t))
  (define s (lens-view (lens 'emulator 'auxiliary 'circuit 'wrapper.soc.cpu.reg_pc) t))
  (define ss (lens-view (lens 'emulator 'auxiliary 'circuit 'wrapper.soc.cpu.cpu_state) t))
  (printf "ckt: ~v ~v emu: ~v ~v\n" c cs s ss))

(define (step-n! n)
  (unless (zero? n)
    (step!)
    (printf "step ~a, " n)
    (show-pcs)
    (step-n! (sub1 n))))

(define (poweroff term)
  (racket/equal? (lens-view (lens 'circuit 'wrapper.pwrmgr_state) term) (bv #b01 2)))

(define ((pc-is pc) term)
  (racket/equal? (lens-view (lens 'circuit 'wrapper.soc.cpu.reg_pc) term) pc))

(define ((state-is state) term)
  (racket/equal? (lens-view (lens 'circuit 'wrapper.soc.cpu.cpu_state) term) state))

(define ((branch-at pc) term)
  (and ((pc-is pc) term) ((state-is (bv #x40 8)) term)))

;; proceed to where we're about to read from UART
(define (step-until! circuit-pred [sync-recv #t])
  (let loop ([i 0])
    (unless (circuit-pred (set-term (current)))
      (step!)
      (when sync-recv
        (sync-overapprox-uart-recv!))
      (printf "step ~a, " i)
      (show-pcs)
      (loop (add1 i)))))

;; insight: we actually don't care what's going on inside the UART,
;; just that the value read by the circuit and the emulated circuit in
;; uart_read is the same

(define (match-abstract! l [name #f])
  (define t (remember+! (list (lens 'circuit l) (lens 'emulator 'auxiliary 'circuit l)) name))
  (clear! t)
  t)

;; the kind of annoying thing is that once we overapproximate what's
;; going on in the recieve side of the uart, it needs to stay
;; overapproximated forever, it never becomes quiescent again
(define (sync-overapprox-uart-recv!)
  (for ([field (list 'wrapper.soc.uart.simpleuart.recv_buf_valid
                     'wrapper.soc.uart.simpleuart.recv_buf_data
                     'wrapper.soc.uart.simpleuart.recv_divcnt
                     'wrapper.soc.uart.simpleuart.recv_pattern
                     'wrapper.soc.uart.simpleuart.recv_state)])
    (match-abstract! field field)))

(define (concretize-branch!)
  (define branch-related (list
                          'wrapper.soc.cpu.reg_pc
                          'wrapper.soc.cpu.decoder_trigger
                          'wrapper.soc.cpu.latched_branch
                          'wrapper.soc.cpu.latched_store
                          'wrapper.soc.cpu.mem_do_rinst))
  (concretize! (lens (list (lens 'circuit branch-related)
                                   (lens 'emulator 'auxiliary 'circuit branch-related)))))

;; proceed to the UART read; returns var
(define (step-past-uart-read! [read-name #f])
  (step-until! (pc-is (bv #x0000007c 32)) #t)

  ;; now, we overapproximate some stuff, so we can loop back later
  (overapproximate!
        (lens (list (lens 'circuit (list 'wrapper.soc.cpu.alu_out_q
                                         'wrapper.soc.cpu.reg_op1
                                         'wrapper.soc.uart.simpleuart.send_divcnt
                                         'wrapper.soc.cpu.count_cycle
                                         'wrapper.soc.cpu.count_instr
                                         (lens 'wrapper.soc.cpu.cpuregs 15)))
                    (lens 'emulator 'auxiliary 'circuit (list 'wrapper.soc.cpu.alu_out_q
                                                               'wrapper.soc.cpu.reg_op1
                                                               'wrapper.soc.uart.simpleuart.send_divcnt
                                                               'wrapper.soc.cpu.count_cycle
                                                               'wrapper.soc.cpu.count_instr
                                                               (lens 'wrapper.soc.cpu.cpuregs 15))))))
  (define pre-read (length (visited)))
  ;; on the branch now
  (step-until! (pc-is (bv #x00000080 32)) #t)
  ;; read is done now, in register x15 (a5), abstract it and then case analyze on whether it returned -1 or not
  (define var (match-abstract! (lens 'wrapper.soc.cpu.cpuregs 15) read-name))
  (define old-pred (set-predicate (current)))
  (cases*! (list (equal? var (bv -1 32))))
  ;; currently in the "no data" case, should be able to get back to start of loop at 0x88
  (step-until! (state-is (bv #x40 8)) #t)
  (concretize-branch!)
  (step-until! (pc-is (bv #x0000007c 32)) #t)
  (step-n! 2)
  (subsumed! pre-read)
  ;; UART read finished
  (step-until! (state-is (bv #x40 8)) #t)
  (concretize-branch!)
  (overapproximate-predicate! old-pred)
  (step-until! (pc-is (bv #x88 32)))
  var)

(define (step-past-uart-write!)
  (step-until! (pc-is (bv #x8c 32)))
  (overapproximate!
        (lens (list (lens 'circuit (list
                                    'wrapper.soc.cpu.alu_out_q
                                    'wrapper.soc.cpu.decoded_imm
                                    'wrapper.soc.cpu.decoded_imm_j
                                    'wrapper.soc.cpu.decoded_rd
                                    'wrapper.soc.cpu.decoded_rs1
                                    'wrapper.soc.cpu.decoded_rs2
                                    'wrapper.soc.cpu.instr_bne
                                    'wrapper.soc.cpu.instr_jal
                                    'wrapper.soc.cpu.is_lui_auipc_jal
                                    'wrapper.soc.cpu.is_lui_auipc_jal_jalr_addi_add_sub
                                    'wrapper.soc.cpu.is_sb_sh_sw
                                    'wrapper.soc.cpu.latched_rd
                                    'wrapper.soc.cpu.mem_rdata_q
                                    'wrapper.soc.cpu.reg_op1
                                    'wrapper.soc.cpu.reg_op2
                                    'wrapper.soc.uart.simpleuart.send_divcnt
                                    'wrapper.soc.cpu.count_cycle
                                    'wrapper.soc.cpu.count_instr
                                    (lens 'wrapper.soc.cpu.cpuregs 15)))
                    (lens 'emulator 'auxiliary 'circuit (list
                                                          'wrapper.soc.cpu.alu_out_q
                                                          'wrapper.soc.cpu.decoded_imm
                                                          'wrapper.soc.cpu.decoded_imm_j
                                                          'wrapper.soc.cpu.decoded_rd
                                                          'wrapper.soc.cpu.decoded_rs1
                                                          'wrapper.soc.cpu.decoded_rs2
                                                          'wrapper.soc.cpu.instr_bne
                                                          'wrapper.soc.cpu.instr_jal
                                                          'wrapper.soc.cpu.is_lui_auipc_jal
                                                          'wrapper.soc.cpu.is_lui_auipc_jal_jalr_addi_add_sub
                                                          'wrapper.soc.cpu.is_sb_sh_sw
                                                          'wrapper.soc.cpu.latched_rd
                                                          'wrapper.soc.cpu.mem_rdata_q
                                                          'wrapper.soc.cpu.reg_op1
                                                          'wrapper.soc.cpu.reg_op2
                                                          'wrapper.soc.uart.simpleuart.send_divcnt
                                                          'wrapper.soc.cpu.count_cycle
                                                          'wrapper.soc.cpu.count_instr
                                                          (lens 'wrapper.soc.cpu.cpuregs 15))))))
  (define pre-write (length (visited)))
  (step-until! (pc-is (bv #x90 32)))
  (define cts-read (match-abstract! (lens 'wrapper.soc.cpu.cpuregs 15) 'cts-read))
  (define old-pred (set-predicate (current)))
  (cases*! (list (not (bvzero? cts-read))))
  ;; no cts case, should be able to get back to start of loop
  (step-until! (state-is (bv #x40 8)))
  (concretize-branch!)
  (step-until! (pc-is (bv #x8c 32)))
  (subsumed! pre-write)
  ;; CTS asserted
  (step-until! (state-is (bv #x40 8)))
  (concretize-branch!)
  (overapproximate-predicate! old-pred)
  (step-until! (pc-is (bv #xa4 32))))

(define (step-to-start-of-main!)
  (concretize!
        (lens 'circuit (list 'wrapper.soc.rom.rom
                             'wrapper.pwrmgr_state
                             'resetn)))

  ;; we want to get rid of the predicate, to simplify subsumption checks
  ;; so we make use of replace and overapproximate-predicate
  ;;
  ;; compute spec state based on circuit state
  (replace! (lens 'emulator 'oracle) (AbsF (lens-view (lens 'term 'circuit) (current))))
  (overapproximate-predicate! #t)

  (overapproximate!
        (lens 'emulator 'auxiliary 'circuit
              (list (field-filter/not (field-filter/or 'resetn 'wrapper.pwrmgr_state
                                                       'wrapper.soc.cpu.cpuregs 'wrapper.soc.ram.ram 'wrapper.soc.fram.fram
                                                       'wrapper.soc.rom.rom 'wrapper.soc.sha1.w_mem))
                    ;; note: we purposefully don't overapproximate the FRAM: we make sure it's zeroed out at the start and end
                    (lens 'wrapper.soc.ram.ram vector-all-elements-lens)
                    (lens 'wrapper.soc.sha1.w_mem vector-all-elements-lens)
                    (lens 'wrapper.soc.cpu.cpuregs vector-all-elements-lens))))

  ;; if CTS was not asserted, then the circuit is still in the "embryo"
  ;; state
  (prepare!) ; prepare first, so we can case-split on inputs _before_ stepping
  #;(step!)
  (cases*! (list (lens-view (lens 'term 'circuit 'cts) (current))))
  (concretize! (lens (list (lens 'circuit (list 'cts))
                                   (lens 'emulator 'auxiliary 'circuit (list 'cts)))))
  ;; we case-split and concretize first before stepping so that we don't
  ;; get terms like (if cts a b), where the two branches are symbolic
  ;; variables, but this can't be optimized by concretize (we'd need a "simplify" tactic)

  (define cpu-free-running-regs
    (field-filter/or
     "cpu.alu_out_q"
     "cpu.decoded_imm"
     "cpu.instr_"
     "cpu.is_jalr"
     "cpu.is_lbu"
     "cpu.is_lui"
     "cpu.is_sll"
     "cpu.is_slti"
     "cpu.mem_rdata_q"))

  (step!)
  (overapproximate!
        (lens (list (lens 'circuit cpu-free-running-regs)
                    (lens 'emulator 'auxiliary 'circuit cpu-free-running-regs))))
  (subsumed! 0)

  ;; "main thread", where CPU has just been reset. we need to concretize
  ;; CTS first so things don't blow up
  (concretize! (lens (list (lens 'circuit (list 'cts))
                                   (lens 'emulator 'auxiliary 'circuit (list 'cts)))))
  (overapproximate-predicate! #t) ; don't need to remember cts anymore

  ;; proceed to uart init
  (step-until! (pc-is (bv #x00000070 32)) #f)

  ;; proceed to right before reading the command
  (step-until! (pc-is (bv #x9cc 32))))

;; takes one goal, when cmd = 2 (OTP), and advances it to the point where hotp is about to be called
;; produces a single goal as output
(define (advance-to-hotp!)
  ;; read ctr
  (for-each (lambda (i)
              (printf "otp, reading ctr[~a]~n" i)
              (step-past-uart-read! (format "ctr[~a]" i)))
            (range spec:CTR-SIZE-BYTES))

  (step-until! (pc-is (bv #x824 32))) ; advance just past the read loop
  ;; clean up counter a bit; this is the one that is read in big-endian format (the c[8] array)
  ;; so the counter value is (concat (swap32 ctr0) (swap32 ctr1))
  ;; we do this before `cint` is computed
  (define ctr0 (match-abstract! (lens 'wrapper.soc.ram.ram 497) 'ctr0))
  (define ctr1 (match-abstract! (lens 'wrapper.soc.ram.ram 498) 'ctr1))
  (define ctr (concat (swap32 ctr0) (swap32 ctr1)))

  ;; going to do case analysis on which region is active, step just past the snez
  (step-until! (pc-is (bv #x83c 32)))
  ;; do this before the case split, so this variable is shared between the branches
  (define secret (remember! (lens 'emulator 'oracle 'secret) 'secret))
  (define max-ctr (remember! (lens 'emulator 'oracle 'max-ctr) 'max-ctr))
  ;; "inverse abstraction function", so we don't have to separately
  ;; prove both the active = 0 and active = 1 cases
  ;;
  ;; we have also very carefully written this in conjunction with AbsF so that
  ;; (R f ci) computes in Rosette to #t, rather than requiring a solver query
  ;; (on every step, for checking the recovery condition)
  (let* ([fram (lens-view (lens 'circuit 'wrapper.soc.fram.fram) (set-term (current)))]
         [fram0 (vector-ref fram 0)])
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 2)
          (if (bvzero? fram0)
              (swap32 (extract 159 128 secret))
              (vector-ref fram 2)) #:use-equalities #t)
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 3)
          (if (bvzero? fram0)
              (swap32 (extract 127 96 secret))
              (vector-ref fram 3)) #:use-equalities #t)
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 4)
          (if (bvzero? fram0)
              (swap32 (extract 95 64 secret))
              (vector-ref fram 4)) #:use-equalities #t)
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 5)
          (if (bvzero? fram0)
              (swap32 (extract 63 32 secret))
              (vector-ref fram 5)) #:use-equalities #t)
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 6)
          (if (bvzero? fram0)
              (swap32 (extract 31 0 secret))
              (vector-ref fram 6)) #:use-equalities #t)
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 8)
          (if (bvzero? fram0)
              (extract 31 0 max-ctr)
              (vector-ref fram 8)) #:use-equalities #t)
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 9)
          (if (bvzero? fram0)
              (extract 63 32 max-ctr)
              (vector-ref fram 9)) #:use-equalities #t)
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 10)
          (if (not (bvzero? fram0))
              (swap32 (extract 159 128 secret))
              (vector-ref fram 10)) #:use-equalities #t)
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 11)
          (if (not (bvzero? fram0))
              (swap32 (extract 127 96 secret))
              (vector-ref fram 11)) #:use-equalities #t)
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 12)
          (if (not (bvzero? fram0))
              (swap32 (extract 95 64 secret))
              (vector-ref fram 12)) #:use-equalities #t)
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 13)
          (if (not (bvzero? fram0))
              (swap32 (extract 63 32 secret))
              (vector-ref fram 13)) #:use-equalities #t)
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 14)
          (if (not (bvzero? fram0))
              (swap32 (extract 31 0 secret))
              (vector-ref fram 14)) #:use-equalities #t)
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 16)
          (if (not (bvzero? fram0))
              (extract 31 0 max-ctr)
              (vector-ref fram 16)) #:use-equalities #t)
    (replace! (lens 'circuit 'wrapper.soc.fram.fram 17)
          (if (not (bvzero? fram0))
              (extract 63 32 max-ctr)
              (vector-ref fram 17)) #:use-equalities #t)
    ;; need to replace counter value that is in emulated circuit as well
    (replace! (lens 'emulator 'auxiliary 'circuit 'wrapper.soc.fram.fram 8)
          (extract 31 0 max-ctr) #:use-equalities #t)
    (replace! (lens 'emulator 'auxiliary 'circuit 'wrapper.soc.fram.fram 9)
          (extract 63 32 max-ctr) #:use-equalities #t))
  ;; don't need secret's "value" anymore, inherent in structure now
  (clear! secret)
  (clear! max-ctr)
  (define active0 (bvzero? (lens-view (lens 'circuit 'wrapper.soc.fram.fram 0) (set-term (current)))))
  (cases! (list active0 (! active0)))

  ;; now, we want to explore both cases up until before the hotp
  ;; computation, and use subsumption to avoid reasoning about the second
  ;; one

  (define (step-case-until-hotp! is-active0)
    (concretize! (lens 'circuit 'wrapper.soc.cpu.cpuregs 10))
    (define case-pred (set-predicate (current)))

    ;; the `if (ctr < max-ctr)` check happens by comparing two 32-bit words, so there are several branches

    ;; first comparison
    (step-until! (branch-at (bv #x8b8 32)))
    (let* ([t (set-term (current))]
           [regs (lens-view (lens 'circuit 'wrapper.soc.cpu.cpuregs) t)]
           [a5 (vector-ref regs 15)]
           [a7 (vector-ref regs 17)])
      (cases! (list (bvult a5 a7) (! (bvult a5 a7)))))

    ;; first bail out case
    (concretize-branch!)
    (overapproximate-predicate! #t) ; don't need this anymore, slows down subsumption checks
    (displayln "otp, ctr > max-ctr, 1st exit")
    (step-past-uart-write!)
    (step-until! poweroff)
    (subsumed! 0)

    ;; second bail out case, beq
    (concretize-branch!)
    (step-until! (branch-at (bv #x8bc 32)))
    (let* ([t (set-term (current))]
           [regs (lens-view (lens 'circuit 'wrapper.soc.cpu.cpuregs) t)]
           [a7 (vector-ref regs 17)]
           [a5 (vector-ref regs 15)])
      (cases! (list (bveq a7 a5) (! (bveq a7 a5)))))

    ;; top 32 bits are equal, check lower 32 bits; and we do the bail-out case first
    (concretize-branch!)
    (step-until! (pc-is (bv #x98c 32))) ; not sure why it reaches pc=0x98c / state=0x40 twice, we want the second one
    (step-until! (state-is (bv #x20 8)))
    (step-until! (branch-at (bv #x98c 32)))
    (let* ([t (set-term (current))]
           [regs (lens-view (lens 'circuit 'wrapper.soc.cpu.cpuregs) t)]
           [a0 (vector-ref regs 10)]
           [a6 (vector-ref regs 16)])
      (cases! (list (! (bvuge a0 a6)) (bvuge a0 a6))))

    ;; second bail out case
    (concretize-branch!)
    (overapproximate-predicate! #t) ; don't need this anymore, slows down subsumption checks
    (displayln "otp, ctr > max-ctr, 2nd exit")
    (step-past-uart-write!)
    (step-until! poweroff)
    (subsumed! 0)

    ;; line up both cases, because both imply !(ctr < max-ctr), and merge them
    ;; line up on the 2nd sw after beq instruction, so more stuff lines up
    (concretize-branch!)
    (step-until! (pc-is (bv #x8c4 32)))
    (define sub-index (length (visited)))
    (overapproximate! (lens 'circuit 'wrapper.soc.cpu.cpuregs 15))
    (overapproximate! (lens 'emulator 'auxiliary 'circuit 'wrapper.soc.cpu.cpuregs 15))
    (define spec-ctr (spec:state-max-ctr (lens-view (lens 'emulator 'oracle) (set-term (current)))))
    (define not-ctr-lt-max-ctr (! (bvult ctr spec-ctr)))
    ;; need to remember the case split for now
    (overapproximate-predicate! (&& case-pred not-ctr-lt-max-ctr))
    (step!)

    ;; now swap goals and merge it
    (switch-goal! 1)
    (concretize-branch!)
    (step-until! (pc-is (bv #x8c4 32)))
    (overapproximate! (lens 'circuit 'wrapper.soc.cpu.cpuregs 15))
    (overapproximate! (lens 'emulator 'auxiliary 'circuit 'wrapper.soc.cpu.cpuregs 15))
    (overapproximate-predicate! (&& case-pred not-ctr-lt-max-ctr))
    (subsumed! sub-index)

    ;; back to the single branch where ctr >= max-ctr

    ;; this is the main proof of otp
    ;; since we've stepped, we can mess with fram now; make it so that it computes easily
    (concretize! (lens 'circuit 'wrapper.soc.fram.fram 0))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 2 10)) (swap32 (extract 159 128 secret)))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 3 11)) (swap32 (extract 127 96 secret)))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 4 12)) (swap32 (extract 95 64 secret)))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 5 13)) (swap32 (extract 63 32 secret)))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 6 14)) (swap32 (extract 31 0 secret)))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 8 16)) (extract 31 0 max-ctr))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 9 17)) (extract 63 32 max-ctr))
    ;; now, go right before commit point, and clean up fram, so that (R f c) computes in Rosette
    (step-until! (pc-is (bv #x8f8 32)))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 16 8)) (swap32 ctr1))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 17 9)) (swap32 ctr0))
    ;; need to replace spec as well, it has a redundant ite (we know counter is less than ...)
    (step-until! (pc-is (bv #x8fc 32)))
    (step-until! (state-is (bv #x40 8)))
    (replace! (lens 'emulator 'oracle 'max-ctr) ctr)
    (concretize! (lens 'circuit 'wrapper.soc.fram.fram 0))
    ;; at this point, (R f c) computes again
    ;; replace emulator cached otp value to not have (ite ...)
    (replace! (lens 'emulator 'auxiliary 'otp-value) (hotp secret ctr)) ; this actually works, just takes a couple seconds
    ;; now, we don't need the predicate anymore
    (overapproximate-predicate! #t)

    ;; step until right before the call to hotp()
    (step-until! (pc-is (bv #x928 32)))
    ;; we are going to do subsumption here, so clean up all of fram
    (overapproximate! (lens 'circuit 'wrapper.soc.fram.fram 0))
    ;; clean up copied ctr
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 8 16)) (swap32 ctr1))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 9 17)) (swap32 ctr0))
    ;; clean up secret as well
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 10 2)) (swap32 (extract 159 128 secret)))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 11 3)) (swap32 (extract 127 96 secret)))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 12 4)) (swap32 (extract 95 64 secret)))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 13 5)) (swap32 (extract 63 32 secret)))
    (replace! (lens 'circuit 'wrapper.soc.fram.fram (if is-active0 14 6)) (swap32 (extract 31 0 secret)))
    ;; clean up CPU registers
    (overapproximate!
          (lens (list (lens 'circuit (list 'wrapper.soc.cpu.mem_wdata
                                           (lens 'wrapper.soc.cpu.cpuregs (list 6 13 16 17 28))))
                      (lens 'emulator 'auxiliary 'circuit (list 'wrapper.soc.cpu.mem_wdata
                                                                 (lens 'wrapper.soc.cpu.cpuregs (list 6 13 16 17 28))))))))

  (step-case-until-hotp! #t)
  (define before-hotp-index (length (visited)))
  (step!)
  (switch-goal! 1)
  (step-case-until-hotp! #f)
  (subsumed! before-hotp-index))

(define (advance-past-hotp!)
  ;; get the counter arg and the secret that the emulator used in the
  ;; (spec:otp ...) call
  (define ctr-arg
    (let ([ram (lens-view (lens 'circuit 'wrapper.soc.ram.ram) (set-term (current)))])
      (concat (swap32 (vector-ref ram 497))
              (swap32 (vector-ref ram 498)))))
  (define secret (lens-view (lens 'emulator 'oracle 'secret) (set-term (current))))
  (define arg1 (message->blocks (pad-message (hmac-sha1-arg1 secret ctr-arg))))
  (define imp (imp-init (first arg1)))
  (define (inject-circuit!)
    (racket/for
     ([lens-pair impl-spec-lenses])
     (match-define (list _ circuit-lens imp-lens) lens-pair)
     (replace! (lens 'circuit circuit-lens) (lens-view imp-lens imp))))
  (define (abstract-circuit!)
    (racket/for/list
     ([lens-pair impl-spec-lenses])
     (match-define (list name circuit-lens imp-lens) lens-pair)
     (define var (remember! (lens 'circuit circuit-lens) name))
     (set! imp (lens-set (lens imp-lens) imp var))
     var))
  (define (step-imp!)
    (set! imp (imp-step imp)))
  (define (sync!)
    (subst! (lens 'circuit (list #rx"sha1\\.([a-e]|h[0-4])" (lens 'wrapper.soc.sha1.w_mem vector-all-elements-lens)))))
  (define (abstract-step-subst!)
    ;; abstract
    (define vars (abstract-circuit!))
    ;; step
    (printf "abstract-step-subst pre:, sha1 state = ~v~n" (lens-view (lens 'circuit 'wrapper.soc.sha1.state) (set-term (current))))
    (step!)
    (printf "abstract-step-subst post:, sha1 state = ~v~n" (lens-view (lens 'circuit 'wrapper.soc.sha1.state) (set-term (current))))
    (sync-overapprox-uart-recv!)
    (step-imp!)
    ;; subst
    (inject-circuit!)
    ;; passing a lens here is important, so we don't traverse large terms
    ;; in the circuit/emulator where this isn't needed
    (sync!)
    (racket/for
     ([var vars])
     (clear! var)))
  (define (step-past-sha-block!)
    ;; step until SHA1 accelerator is about to start processing the first block
    (printf "step-past-sha-block: stepping until sha1.state = #b01 (currently ~v)~n"
            (lens-view (lens 'circuit 'wrapper.soc.sha1.state) (set-term (current))))
    (step-until!
     (lambda (term) (racket/equal? (lens-view (lens 'circuit 'wrapper.soc.sha1.state) term) (bv #b01 2))))
    ;; initial inject
    (inject-circuit!)
    (sync!) ; need this for second block
    ; now, a bunch of times: abstract, step, inject, subst, clear, overapproximate
    (let loop ([n 0])
      (when (racket/equal? (lens-view (lens 'circuit 'wrapper.soc.sha1.state) (set-term (current))) (bv #b01 2))
        (printf "sha stepping, n = ~v, t = ~v~n" n (lens-view (lens 'circuit 'wrapper.soc.sha1.t) (set-term (current))))
        (abstract-step-subst!)
        (loop (add1 n))))
    ;; do everything one last time, because we're about to update H[0-5]
    (abstract-step-subst!)
    (printf "step-past-sha-block: abstracting one last time")
    ;; and then abstract one last time
    (abstract-circuit!))

  ;; deal with HMAC
  (step-past-sha-block!)
  (set! imp (imp-next imp (second arg1)))
  (step-past-sha-block!)
  (define arg2 (message->blocks (pad-message (hmac-sha1-arg2 imp secret))))
  (printf "arg2: ~v~narg2 symbolics: ~v~n"
          arg2
          (symbolics arg2))
  (set! imp (imp-init (first arg2)))
  (step-past-sha-block!)
  (set! imp (imp-next imp (second arg2)))
  (step-past-sha-block!)

  (define hmac-output (imp-finalize imp))

  ;; proceed up until just before call to constant-time mod 10^6
  (step-until! (pc-is (bv #x584 32)))

  ;; at this point, the argument to the ct_mod1000000 is in register a0 (x10)
  (define s (dt hmac-output))
  (replace! (lens 'circuit 'wrapper.soc.cpu.cpuregs 10) s)
  ;; we could abstract this once more, to simplify verifying correctness of modulo
  ;; then it takes ~ 75 seconds to verify that ct_mod1000000 implements mod
  ;; but apparently, the solver is faster when we don't abstract this,
  ;; taking only ~ 50 seconds

  ;; return from hotp/ct_mod1000000
  (step-until! (pc-is (bv #x92c 32)))
  (define spec-otp (bvurem s (bv (expt 10 DIGITS) 32)))
  (define a0 (lens-view (lens 'circuit 'wrapper.soc.cpu.cpuregs 10) (set-term (current))))

  (replace! (lens 'circuit 'wrapper.soc.cpu.cpuregs 10) spec-otp) ; takes 50 seconds, but it works
  (subst! (lens 'circuit 'wrapper.soc.cpu.cpuregs 10))
  (clear!) ; no need to remember variables anymore

  ;; step one instruction past when emulator injects the HOTP value
  (step-until! (pc-is (bv #x934 32)))
  ;; into s0 register
  (match-abstract! (lens 'wrapper.soc.cpu.cpuregs 8) 'hotp))

(define (case-get-otp!)
  (advance-to-hotp!)
  (advance-past-hotp!)

  ;; write 5 bytes and then exit
  (for-each (lambda _ (step-past-uart-write!)) (range 5))
  (step-until! poweroff)
  (subsumed! 0))

(define (case-audit!)
  ;; step until right after snez
  (step-until! (pc-is (bv #x778 32)))
  (define active0 (bvzero? (lens-view (lens 'circuit 'wrapper.soc.fram.fram 0) (set-term (current)))))
  (cases! (list active0 (! active0)))

  (define (audit-active-case! is-active0)
    (concretize! (lens 'circuit 'wrapper.soc.cpu.cpuregs 15))
    ;; make emulator fram match
    (let ([fram (lens-view (lens 'circuit 'wrapper.soc.fram.fram) (set-term (current)))])
      (replace! (lens 'emulator 'auxiliary 'circuit 'wrapper.soc.fram.fram 8) (vector-ref fram (if is-active0 8 16)))
      (replace! (lens 'emulator 'auxiliary 'circuit 'wrapper.soc.fram.fram 9) (vector-ref fram (if is-active0 9 17))))
    (overapproximate-predicate! #t)
    (for-each (lambda (i)
                (printf "audit, active=~a, write ctr[~a]\n" (if is-active0 0 1) i)
                (step-past-uart-write!)) (range 8))
    (step-until! poweroff)
    (subsumed! 0))
  (audit-active-case! #t)
  (audit-active-case! #f))

(define (case-set-secret!)
  (for-each (lambda (i)
              (printf "set-secret, reading new-secret[~a]~n" i)
              (step-past-uart-read! (format "new-secret[~a]" i)))
            (range spec:SECRET-SIZE-BYTES))

  ;; clean up secret
  (step-until! (pc-is (bv #x6dc 32)))
  (define secret0 (match-abstract! (lens 'wrapper.soc.ram.ram 499) 'secret0))
  (define secret1 (match-abstract! (lens 'wrapper.soc.ram.ram 500) 'secret1))
  (define secret2 (match-abstract! (lens 'wrapper.soc.ram.ram 501) 'secret2))
  (define secret3 (match-abstract! (lens 'wrapper.soc.ram.ram 502) 'secret3))
  (define secret4 (match-abstract! (lens 'wrapper.soc.ram.ram 503) 'secret4))

  ;; step until right after snez
  (step-until! (pc-is (bv #x6f0 32)))
  (define active0 (bvzero? (lens-view (lens 'circuit 'wrapper.soc.fram.fram 0) (set-term (current)))))
  (cases! (list active0 (! active0)))

  (define (set-secret-active-case! is-active0)
    (concretize! (lens 'circuit 'wrapper.soc.cpu.cpuregs 12))
    (let ([fram (lens-view (lens 'circuit 'wrapper.soc.fram.fram) (set-term (current)))])
      (replace! (lens 'emulator 'oracle 'max-ctr)
            (concat
             (vector-ref fram (if is-active0 9 17))
             (vector-ref fram (if is-active0 8 16)))))
    (step-until! (pc-is (bv #x744 32)))
    (concretize! (lens 'circuit 'wrapper.soc.cpu.cpuregs 15))
    (step-until! (pc-is (bv #x748 32))) ; after the store, can forget the predicate
    (overapproximate-predicate! #t)
    (step-past-uart-write!)
    (step-until! poweroff)
    (subsumed! 0))

  (set-secret-active-case! #t)
  (set-secret-active-case! #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(step-to-start-of-main!)

(define cmd (step-past-uart-read! 'cmd))

;; OTP?
(step-until! (branch-at (bv #x9d4 32)) #t)
;; at first branch in main, is it a otp command?
(cases*! (list (equal? (bvand (bv #xff 32) cmd) (bv 2 32))))
(concretize-branch!)
(overapproximate-predicate! #t) ; don't need to remember cmd=2
(case-get-otp!)

;; AUDIT?
(concretize-branch!)
(overapproximate-predicate! #t)
(step-until! (branch-at (bv #x9dc 32)) #t)
;; at second branch in main, is it a audit command?
(cases*! (list (equal? (bvand (bv #xff 32) cmd) (bv 3 32))))
(concretize-branch!)
(overapproximate-predicate! #t)
(case-audit!)

;; SET-SECRET?
(concretize-branch!)
(overapproximate-predicate! #t)
(step-until! (branch-at (bv #x9e4 32)) #t)
;; at second branch in main, is it a set-secret command?
(cases*! (list (equal? (bvand (bv #xff 32) cmd) (bv 1 32))))
(concretize-branch!)
(overapproximate-predicate! #t)
(case-set-secret!)

;; INVALID?
(concretize-branch!)
(step-until! poweroff)
(subsumed! 0)
