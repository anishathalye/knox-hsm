#lang knox/security

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:emulator "emulator.rkt"
#:R R
;; #:skip-final-check #t

(require
 "shared.rkt"
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
  (step-until! (pc-is (bv #x00000078 32)) #t)

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
  (step-until! (pc-is (bv #x0000007c 32)) #t)
  ;; read is done now, in register x15 (a5), abstract it and then case analyze on whether it returned -1 or not
  (define var (match-abstract! (lens 'wrapper.soc.cpu.cpuregs 15) read-name))
  (define old-pred (set-predicate (current)))
  (cases*! (list (equal? var (bv -1 32))))
  ;; currently in the "no data" case, should be able to get back to start of loop at 0x88
  (step-until! (state-is (bv #x40 8)) #t)
  (concretize-branch!)
  (step-until! (pc-is (bv #x00000078 32)) #t)
  (step-n! 2)
  (subsumed! pre-read)
  ;; UART read finished
  (step-until! (state-is (bv #x40 8)) #t)
  (concretize-branch!)
  (overapproximate-predicate! old-pred)
  (step-until! (pc-is (bv #x84 32)))
  var)

(define (step-past-uart-write!)
  (step-until! (pc-is (bv #x88 32)))
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
  (step-until! (pc-is (bv #x8c 32)))
  (define cts-read (match-abstract! (lens 'wrapper.soc.cpu.cpuregs 15) 'cts-read))
  (define old-pred (set-predicate (current)))
  (cases*! (list (not (bvzero? cts-read))))
  ;; no cts case, should be able to get back to start of loop
  (step-until! (state-is (bv #x40 8)))
  (concretize-branch!)
  (step-until! (pc-is (bv #x88 32)))
  (subsumed! pre-write)
  ;; CTS asserted
  (step-until! (state-is (bv #x40 8)))
  (concretize-branch!)
  (overapproximate-predicate! old-pred)
  (step-until! (pc-is (bv #xa0 32))))

(define (step-to-start-of-main!)
  (concretize!
        (lens 'circuit (list 'wrapper.soc.rom.rom
                             'wrapper.pwrmgr_state)))

  ;; we want to get rid of the predicate, to simplify subsumption checks
  ;; so we make use of replace and overapproximate-predicate
  ;;
  ;; compute spec state based on circuit state
  (replace! (lens 'emulator 'oracle) (AbsF (lens-view (lens 'term 'circuit) (current))))
  (overapproximate-predicate! #t)

  (overapproximate!
        (lens 'emulator 'auxiliary 'circuit
              (list (field-filter/not (field-filter/or 'wrapper.pwrmgr_state
                                                       'wrapper.soc.cpu.cpuregs 'wrapper.soc.ram.ram 'wrapper.soc.fram.fram
                                                       'wrapper.soc.rom.rom))
                    ;; note: we purposefully don't overapproximate the FRAM: we make sure it's zeroed out at the start and end
                    (lens 'wrapper.soc.ram.ram vector-all-elements-lens)
                    (lens 'wrapper.soc.cpu.cpuregs vector-all-elements-lens))))

  ;; if CTS was not asserted, then the circuit is still in the "embryo"
  ;; state
  (prepare!) ; prepare first, so we can case-split on inputs _before_ stepping
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
  (step-until! (pc-is (bv #x0000006c 32)) #f))

(step-to-start-of-main!)

(define cmd (step-past-uart-read! 'cmd))
#;(define slot (step-past-uart-read! 'slot))

;; add cmd?
(step-until! (branch-at (bv #x1a4 32)) #t)
(cases*! (list (bveq (bvand (bv #xff 32) cmd) (bv 1 32))))
(concretize-branch!)
;; XXX add case
(step-past-uart-read!)
(step-past-uart-read!)
(step-past-uart-read!)
(step-past-uart-read!)
(step-past-uart-write!)
(step-until! poweroff)
(subsumed! 0)

;; get cmd?
(concretize-branch!)
(step-until! (branch-at (bv #x1ac 32)) #t)
(cases*! (list (bveq (bvand (bv #xff 32) cmd) (bv 2 32))))
(concretize-branch!)
(step-past-uart-write!)
(step-past-uart-write!)
(step-past-uart-write!)
(step-past-uart-write!)
(step-until! poweroff)
(subsumed! 0)

;; invalid
(concretize-branch!)
(step-until! poweroff)
(subsumed! 0)
