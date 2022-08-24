#lang knox/security

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:emulator "emulator.rkt"
#:R R
;; #:skip-final-check #t

(require
 "shared.rkt"
 (only-in "../spec/spec-sha256.rkt" pad-message sha256-init sha256-finalize)
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
                        'wrapper.soc.sha256.k
                        'wrapper.pwrmgr_state)))

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
                                                  'wrapper.soc.rom.rom 'wrapper.soc.sha256.k 'wrapper.soc.sha256.w_mem))
               ;; note: we purposefully don't overapproximate the FRAM: we make sure it's zeroed out at the start and end
               (lens 'wrapper.soc.ram.ram vector-all-elements-lens)
               (lens 'wrapper.soc.sha256.k vector-all-elements-lens)
               (lens 'wrapper.soc.sha256.w_mem vector-all-elements-lens)
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
(step-until! (branch-at (bv #x54c 32)) #t)
;; at first branch in main, is it a set-secret command?
(cases*! (list (not (equal? (bvand (bv #xff 32) cmd) (bv 1 32)))))
(concretize-branch!)
(step-until! (branch-at (bv #x554 32)) #t)
(cases*! (list (not (equal? (bvand (bv #xff 32) cmd) (bv 2 32))))) ;; get-hash or invalid?
;; invalid cmd
(concretize-branch!)
(step-until! poweroff)
(subsumed! 0)

;; hash case
(concretize-branch!)
(overapproximate-predicate! #t)
(for-each (lambda (i)
            (printf "store, reading message[~a]~n" i)
            (step-past-uart-read! (format "message[~a]" i)))
          (range spec:MESSAGE-SIZE-BYTES))
(step-until! (pc-is (bv #x4ac 32)))
;; do this before the case split, so this variable is shared between the branches
(define secret (remember! (lens 'emulator 'oracle) 'secret))
;; "inverse abstraction function", so we don't have to separately
;; prove both the active = 0 and active = 1 cases
;;
;; we have also very carefully written this in conjunction with AbsF so that
;; (R f ci) computes in Rosette to #t, rather than requiring a solver query
;; (on every step, for checking the recovery condition)
(let ([fram (lens-view (lens 'circuit 'wrapper.soc.fram.fram) (set-term (current)))])
  (replace! (lens 'circuit 'wrapper.soc.fram.fram 1)
        (if (bvzero? (vector-ref fram 0))
            (swap32 (extract 159 128 secret))
            (vector-ref fram 1)) #:use-equalities #t)
  (replace! (lens 'circuit 'wrapper.soc.fram.fram 2)
        (if (bvzero? (vector-ref fram 0))
            (swap32 (extract 127 96 secret))
            (vector-ref fram 2)) #:use-equalities #t)
  (replace! (lens 'circuit 'wrapper.soc.fram.fram 3)
        (if (bvzero? (vector-ref fram 0))
            (swap32 (extract 95 64 secret))
            (vector-ref fram 3)) #:use-equalities #t)
  (replace! (lens 'circuit 'wrapper.soc.fram.fram 4)
        (if (bvzero? (vector-ref fram 0))
            (swap32 (extract 63 32 secret))
            (vector-ref fram 4)) #:use-equalities #t)
  (replace! (lens 'circuit 'wrapper.soc.fram.fram 5)
        (if (bvzero? (vector-ref fram 0))
            (swap32 (extract 31 0 secret))
            (vector-ref fram 5)) #:use-equalities #t)
  (replace! (lens 'circuit 'wrapper.soc.fram.fram 6)
        (if (not (bvzero? (vector-ref fram 0)))
            (swap32 (extract 159 128 secret))
            (vector-ref fram 6)) #:use-equalities #t)
  (replace! (lens 'circuit 'wrapper.soc.fram.fram 7)
        (if (not (bvzero? (vector-ref fram 0)))
            (swap32 (extract 127 96 secret))
            (vector-ref fram 7)) #:use-equalities #t)
  (replace! (lens 'circuit 'wrapper.soc.fram.fram 8)
        (if (not (bvzero? (vector-ref fram 0)))
            (swap32 (extract 95 64 secret))
            (vector-ref fram 8)) #:use-equalities #t)
  (replace! (lens 'circuit 'wrapper.soc.fram.fram 9)
        (if (not (bvzero? (vector-ref fram 0)))
            (swap32 (extract 63 32 secret))
            (vector-ref fram 9)) #:use-equalities #t)
  (replace! (lens 'circuit 'wrapper.soc.fram.fram 10)
        (if (not (bvzero? (vector-ref fram 0)))
            (swap32 (extract 31 0 secret))
            (vector-ref fram 10)) #:use-equalities #t))
;; don't need secret anymore, inherent in structure now
(clear! secret)
(let ([ax (lens-view (lens 'circuit 'wrapper.soc.cpu.cpuregs 12) (set-term (current)))])
  (cases! (map (lambda (v) (equal? ax v)) (list (bv 0 32) (bv 1 32)))))
;; case active = x
(concretize! (lens 'circuit 'wrapper.soc.cpu.cpuregs 12))
(step-until! (pc-is (bv #x4e8 32)))
;; clean up stack contents
(replace! (lens 'circuit 'wrapper.soc.ram.ram 487) (swap32 (extract 159 128 secret)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 488) (swap32 (extract 127 96 secret)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 489) (swap32 (extract 95 64 secret)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 490) (swap32 (extract 63 32 secret)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 491) (swap32 (extract 31 0 secret)))
;; don't need pred now, we've already made use of the case split info,
;; and we can overapproximate it to allow for subsumption checks later to
;; work
(overapproximate-predicate! #t)
;; clean up message a bit
(match-abstract! (lens 'wrapper.soc.ram.ram 492) 'message0)
(match-abstract! (lens 'wrapper.soc.ram.ram 493) 'message1)
(match-abstract! (lens 'wrapper.soc.ram.ram 494) 'message2)
(match-abstract! (lens 'wrapper.soc.ram.ram 495) 'message3)
(match-abstract! (lens 'wrapper.soc.ram.ram 496) 'message4)
(match-abstract! (lens 'wrapper.soc.ram.ram 497) 'message5)
(match-abstract! (lens 'wrapper.soc.ram.ram 498) 'message6)
(match-abstract! (lens 'wrapper.soc.ram.ram 499) 'message7)
(overapproximate!
      (lens (list (lens 'circuit (list 'wrapper.soc.cpu.mem_wdata
                                       (lens 'wrapper.soc.cpu.cpuregs (list 14 15))))
                  (lens 'emulator 'auxiliary 'circuit (list 'wrapper.soc.cpu.mem_wdata
                                                             (lens 'wrapper.soc.cpu.cpuregs (list 14 15)))))))
(define ready-to-hash-index (length (visited))) ; save for later
(step-until!
 (lambda (term) (racket/equal? (lens-view (lens 'circuit 'wrapper.soc.sha256.state) term) (bv #b01 2))))
(define c (lens-view (lens 'circuit) (set-term (current))))
(define f (lens-view (lens 'emulator 'oracle) (set-term (current))))

;; initial inject
(define block
  (let ([ram (lens-view (lens 'circuit 'wrapper.soc.ram.ram) (set-term (current)))])
    (pad-message (concat
                  secret
                  (concat (swap32 (vector-ref ram 492))
                          (swap32 (vector-ref ram 493))
                          (swap32 (vector-ref ram 494))
                          (swap32 (vector-ref ram 495))
                          (swap32 (vector-ref ram 496))
                          (swap32 (vector-ref ram 497))
                          (swap32 (vector-ref ram 498))
                          (swap32 (vector-ref ram 499)))))))
(define imp (imp-init (sha256-init) block))
(define (inject-circuit!)
  (racket/for
   ([lens-pair impl-spec-lenses])
   (match-define (list _ circuit-lens imp-lens) lens-pair)
   (replace! (lens 'circuit circuit-lens) (lens-view imp-lens imp))))
(define (abstract-circuit!)
  (racket/for
   ([lens-pair impl-spec-lenses])
   (match-define (list name circuit-lens imp-lens) lens-pair)
   (define var (remember! (lens 'circuit circuit-lens) name))
   (set! imp (lens-set (lens imp-lens) imp var))))
(define (step-imp!)
  (set! imp (imp-step imp)))
(define (abstract-step-subst!)
  ;; abstract
  (abstract-circuit!)
  ;; step
  (step!)
  (sync-overapprox-uart-recv!)
  (step-imp!)
  ;; subst
  (inject-circuit!)
  ;; passing a lens here is important, so we don't traverse large terms
  ;; in the circuit/emulator where this isn't needed
  (subst! (lens 'circuit (list #rx"sha256\\.[a-h]" (lens 'wrapper.soc.sha256.w_mem vector-all-elements-lens))))
  (clear!)
  ;; throw away emulator state we don't care about; this makes a big
  ;; difference in helping us not slow down every cycle
  (overapproximate! (lens 'emulator 'auxiliary 'circuit
                                  (list #rx"sha256\\.[a-h]" (lens 'wrapper.soc.sha256.w_mem vector-all-elements-lens)))))

;; initial inject
(inject-circuit!)
; now, a bunch of times: abstract, step, inject, subst, clear, overapproximate
(let loop ([n 0])
  (when (racket/equal? (lens-view (lens 'circuit 'wrapper.soc.sha256.state) (set-term (current))) (bv #b01 2))
    (printf "sha stepping, n = ~v, t = ~v~n" n (lens-view (lens 'circuit 'wrapper.soc.sha256.t) (set-term (current))))
    (abstract-step-subst!)
    (loop (add1 n))))
;; abstract one last time, because we're about to update H[0-7]
(abstract-circuit!)
(step!)
(sync-overapprox-uart-recv!)
;; return from sha256_digest, where digest in RAM is populated
(step-until! (pc-is (bv #x4ec 32)))
(step-until! (state-is (bv #x08 8)))

;; okay, now we want to sync up the impl and spec

(define imp-digest (sha256-finalize (sha256-add (imp->sha256-state imp) (sha256-init))))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 513) (swap32 (extract 255 224 imp-digest)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 514) (swap32 (extract 223 192 imp-digest)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 515) (swap32 (extract 191 160 imp-digest)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 516) (swap32 (extract 159 128 imp-digest)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 517) (swap32 (extract 127 96 imp-digest)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 518) (swap32 (extract 95 64 imp-digest)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 519) (swap32 (extract 63 32 imp-digest)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 520) (swap32 (extract 31 0 imp-digest)))
(subst! (lens 'circuit 'wrapper.soc.ram.ram (list 513 514 515 516 517 518 519 520)))
(clear!)
;; they're matched up now; we actually don't care about the exact values anymore, better to abstract
(match-abstract! (lens 'wrapper.soc.ram.ram 513) 'h0)
(match-abstract! (lens 'wrapper.soc.ram.ram 514) 'h1)
(match-abstract! (lens 'wrapper.soc.ram.ram 515) 'h2)
(match-abstract! (lens 'wrapper.soc.ram.ram 516) 'h3)
(match-abstract! (lens 'wrapper.soc.ram.ram 517) 'h4)
(match-abstract! (lens 'wrapper.soc.ram.ram 518) 'h5)
(match-abstract! (lens 'wrapper.soc.ram.ram 519) 'h6)
(match-abstract! (lens 'wrapper.soc.ram.ram 520) 'h7)

(for-each
 (lambda (i)
   (printf "hash, writing hash[~a]~n" i)
   (step-past-uart-write!))
 (range 32))

(step-until! poweroff)
(subsumed! 0)

;; get hash, but case where active region is 1; proof via subsumption
(concretize! (lens 'circuit 'wrapper.soc.cpu.cpuregs 12))
(step-until! (pc-is (bv #x4e8 32)))
;; clean up stack contents, to match the thing we're going to be subsumed by (faster computation)
(replace! (lens 'circuit 'wrapper.soc.ram.ram 487) (swap32 (extract 159 128 secret)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 488) (swap32 (extract 127 96 secret)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 489) (swap32 (extract 95 64 secret)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 490) (swap32 (extract 63 32 secret)))
(replace! (lens 'circuit 'wrapper.soc.ram.ram 491) (swap32 (extract 31 0 secret)))
;; don't need pred now
(overapproximate-predicate! #t)
(overapproximate!
      (lens (list (lens 'circuit (list 'wrapper.soc.cpu.mem_wdata
                                       (lens 'wrapper.soc.cpu.cpuregs (list 14 15))))
                  (lens 'emulator 'auxiliary 'circuit (list 'wrapper.soc.cpu.mem_wdata
                                                             (lens 'wrapper.soc.cpu.cpuregs (list 14 15)))))))
;; clean up message
(match-abstract! (lens 'wrapper.soc.ram.ram 492) 'message0)
(match-abstract! (lens 'wrapper.soc.ram.ram 493) 'message1)
(match-abstract! (lens 'wrapper.soc.ram.ram 494) 'message2)
(match-abstract! (lens 'wrapper.soc.ram.ram 495) 'message3)
(match-abstract! (lens 'wrapper.soc.ram.ram 496) 'message4)
(match-abstract! (lens 'wrapper.soc.ram.ram 497) 'message5)
(match-abstract! (lens 'wrapper.soc.ram.ram 498) 'message6)
(match-abstract! (lens 'wrapper.soc.ram.ram 499) 'message7)
(subsumed! ready-to-hash-index) ; this works even without the replaces, but it's probably faster with replace above

;; store case
;; begin store case proof
(concretize-branch!)
;; don't need to remember that cmd = 1 anymore
(overapproximate-predicate! #t)
(for-each (lambda (i)
        (printf "store, reading secret[~a]~n" i)
        (step-past-uart-read! (format "secret[~a]" i)))
    (range spec:SECRET-SIZE-BYTES))
(step-until! (pc-is (bv #x404 32))) ; step just past the seqz
(let ([ax (lens-view (lens 'circuit 'wrapper.soc.cpu.cpuregs 13) (set-term (current)))])
  (cases! (map (lambda (v) (equal? ax v)) (list (bv 0 32) (bv 1 32)))))
;; case active = x
(concretize! (lens 'circuit 'wrapper.soc.cpu.cpuregs 13))
(step-past-uart-write!)
(step-until! poweroff)
(subsumed! 0)
;; case active = !x
(concretize! (lens 'circuit 'wrapper.soc.cpu.cpuregs 13))
(step-past-uart-write!)
(step-until! poweroff)
(subsumed! 0)
;; end store case proof
