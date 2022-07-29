#lang knox/correctness

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:driver "../spec/driver.rkt"
#:R R
#:hints hints
;; #:only 'get-hash
;; #:without-yield #t
;; #:without-crashes #t
#:verbose #t

(require
 "shared.rkt"
 "../spec/spec.rkt"
 "../spec/spec-sha256.rkt"
 racket/match
 knox/correctness/hint
 knox/correctness/checker
 rosette/safe
 yosys/meta
 knox/spec
 knox/circuit
 (only-in rosette/base/core/polymorphic ite)
 rosutil)

(define (hints method c1 f1 f-out f2)
  (match method
    [`(set-secret ,secret)
     (extend-hintdb
      common-hintdb
      [overapproximate-boot-pc (overapproximate-pc! (R f1 c1))]
      [initial-inject done]
      [pre-tick done]
      [post-tick done]
      [finalize done]
      [maybe-replace-and-merge-after-cases done] ; kind of annoying to figure out how to merge at this point, so don't bother
      [maybe-split-branchless (split-branchless #x404 13)]
      [maybe-merge-after-recv (merge-at-pc #x3f0)])]
    [`(get-hash ,msg)
     (define imp (imp-init (sha256-init) (pad-message (concat f1 msg))))
     (define (inject)
       (for ([el impl-spec-lenses])
         (replace! (lens 'circuit (second el)) (lens-view (third el) imp))))
     (define (abstract)
       (for ([el impl-spec-lenses])
         (define var (remember! (lens 'circuit (second el)) (first el)))
         (set! imp (lens-set (third el) imp var))))
     (define (replace* lens-value #:use-pc [use-pc #f])
       (for ([lvp (in-list lens-value)])
         (replace! (car lvp) (cdr lvp) #:use-pc use-pc)))
     (extend-hintdb
      common-hintdb
      [overapproximate-boot-pc (overapproximate-pc! (R f1 c1))]
      [maybe-split-branchless (split-branchless #x4ac 12)]
      [maybe-merge-after-recv (merge-at-pc #x498)]
      [initial-inject
       (let ([triggered (box #f)])
         (tactic
          (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
          (when (and (not (unbox triggered)) (equal? (get-field ckt 'wrapper.soc.sha256.state) (bv #b01 2)))
            (displayln "injecting sha256 initial state")
            (set-box! triggered #t)
            (inject))))]
      [pre-tick
       ;; abstract
       (tactic
        (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
        (when
            (equal? (get-field ckt 'wrapper.soc.sha256.state) (bv #b01 2))
          (abstract)))]
      [post-tick
       (tactic
        (define st (get-state))
        (define ckt (lens-view (lens 'interpreter 'globals 'circuit) st))
        (when
            (and (not (hash-empty? (checker-state-equalities st)))
                 (or (equal? (get-field ckt 'wrapper.soc.sha256.state) (bv #b01 2))
                     (equal? (get-field ckt 'wrapper.soc.sha256.state) (bv #b10 2)))) ; we did something pre-tick
          ;; step the implementation
          (set! imp (imp-step imp))
          ;; inject into circuit
          (inject)
          ;; substitute all variables
          (subst! (lens 'circuit (list #rx"wrapper.soc.sha256\\.[a-h]" (lens 'wrapper.soc.sha256.w_mem vector-all-elements-lens))))
          (clear!)
          ;; if it's the last step, abstract once more
          (when (equal? (get-field ckt 'wrapper.soc.sha256.state) (bv #b10 2))
            (abstract))))]
      [finalize
       (tactic
        (define value-lens (lens 'interpreter 'environment (list-ref-lens 0) cdr-lens))
        (replace! value-lens (sha256-finalize (sha256-add (imp->sha256-state imp) (sha256-init))))
        (subst! value-lens)
        (clear!))]
      [maybe-replace-and-merge-after-cases
       (tactic
        (define st (get-state))
        (define ckt (lens-view (lens 'interpreter 'globals 'circuit) st))
        (when (and (equal? (get-field ckt 'wrapper.soc.cpu.reg_pc) (bv #x4d4 32)) (equal? (get-field ckt 'wrapper.soc.cpu.cpu_state) (bv #x20 8)))
          (eprintf "pc: ~v state: ~v, rewriting secret and merging~n" (get-field ckt 'wrapper.soc.cpu.reg_pc) (get-field ckt 'wrapper.soc.cpu.cpu_state))
          (define path (checker-state-pc st))
          (eprintf "path condition: ~v~n" path)
          (replace* (list (cons (lens 'circuit 'wrapper.soc.ram.ram 487) (swap32 (extract 159 128 f1)))
                          (cons (lens 'circuit 'wrapper.soc.ram.ram 488) (swap32 (extract 127 96 f1)))
                          (cons (lens 'circuit 'wrapper.soc.ram.ram 489) (swap32 (extract 95 64 f1)))
                          (cons (lens 'circuit 'wrapper.soc.ram.ram 490) (swap32 (extract 63 32 f1)))
                          (cons (lens 'circuit 'wrapper.soc.ram.ram 491) (swap32 (extract 31 0 f1))))
                    #:use-pc #t)
          (overapproximate! (lens 'circuit (list 'wrapper.soc.cpu.mem_wdata
                                                 (lens 'wrapper.soc.cpu.cpuregs (list 12 14 15)))))
          ;; note: we overapproximate to (R f1 c1), not just #t, because otherwise we won't
          ;; be able to prove (R f1 c1); we haven't "overwritten" the secret in the fram,
          ;; we need to remember the relation (we can't just overwrite because we don't know which one it is)
          (overapproximate-pc! (R f1 c1))
          (merge!)))])]))

(define common-hintdb
  (make-hintdb
   [concretize-init (concretize! (lens 'circuit (field-filter/or "pwrmgr_state" "resetn" "rom" "sha256.k")) #:use-pc #t)]
   [concretize-uart
    (concretize! (lens 'circuit (field-filter/or 'wrapper.soc.uart.simpleuart.recv_buf_data 'wrapper.soc.uart.simpleuart.recv_pattern)))]
   [maybe-split-pc
    (tactic
     (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
     (define pc (get-field ckt 'wrapper.soc.cpu.reg_pc))
     (match pc
       [(expression (== ite) c _ _)
        (eprintf "case splitting on ~v~n" c)
        (case-split! (list c (! c)))
        (concretize! (lens 'circuit "wrapper.soc.cpu.") #:piecewise #t #:use-pc #t)]
       [_ (void)]))]
   [maybe-concretize-cpu
    (tactic
     (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
     (when (symbolic? (get-field ckt 'wrapper.soc.cpu.decoder_trigger))
       (concretize! (lens 'circuit (field-filter/or 'wrapper.soc.cpu.decoder_trigger 'wrapper.soc.cpu.latched_branch
                                                    'wrapper.soc.cpu.latched_store 'wrapper.soc.cpu.mem_do_rinst))
                    #:use-pc #t)))]
   [uart-fp (fixpoint 56 #t 9
                      (lens (list 'wrapper.soc.cpu.reg_pc 'wrapper.soc.cpu.decoder_trigger 'wrapper.soc.cpu.latched_branch
                                  'wrapper.soc.cpu.latched_store 'wrapper.soc.cpu.mem_do_rinst))
                      (lens (list 'wrapper.soc.cpu.count_cycle 'wrapper.soc.cpu.count_instr
                                  'wrapper.soc.uart.simpleuart.send_divcnt)))]
   [overapproximate-uart (overapproximate! (lens 'circuit (field-filter/or "send_divcnt")))]
   [overapproximate-boot (overapproximate! (lens 'circuit (field-filter/or "count_cycle" "count_instr" "send_divcnt")))]
   [overapproximate-send-merge (overapproximate! (lens 'circuit (field-filter/or "count_cycle" "count_instr" "send_divcnt")))]
   [overapproximate-recv-merge (overapproximate! (lens 'circuit (field-filter/or "count_cycle" "count_instr")))]
   [merge (merge! (lambda (s)
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

(define (split-branchless pc reg)
  (tactic
   (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
   (when (and (equal? (get-field ckt 'wrapper.soc.cpu.reg_pc) (bv pc 32)) (equal? (get-field ckt 'wrapper.soc.cpu.cpu_state) (bv #x20 8)))
     (let ([ax (lens-view (lens 'wrapper.soc.cpu.cpuregs reg) ckt)])
       (displayln "case split on active = 0 or active = 1")
       (case-split! (map (lambda (v) (equal? ax v)) (list (bv 0 32) (bv 1 32))))
       (concretize! (lens 'circuit 'wrapper.soc.cpu.cpuregs reg) #:use-pc #t)))))

(define (merge-at-pc pc)
  (tactic
   (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
   (when (and (equal? (get-field ckt 'wrapper.soc.cpu.reg_pc) (bv pc 32)) (equal? (get-field ckt 'wrapper.soc.cpu.cpu_state) (bv #x20 8)))
     (let ()
       (eprintf "pc: ~v state: ~v, merging~n" (get-field ckt 'wrapper.soc.cpu.reg_pc) (get-field ckt 'wrapper.soc.cpu.cpu_state))
       (merge!)))))
