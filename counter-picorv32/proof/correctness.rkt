#lang knox/correctness

#:spec "../spec/spec.rkt"
#:circuit "circuit.rkt"
#:driver "../spec/driver.rkt"
#:R R
#:hints hints
;; #:only 'add
;; #:without-crashes #t
;; #:without-yield #t
#:verbose #t

(require
 "shared.rkt"
 racket/match
 knox/correctness/hint
 rosette/safe
 yosys/meta
 knox/spec
 knox/circuit
 (only-in rosette/base/core/polymorphic ite)
 rosutil)

(define (hints method c1 f1 f-out f2)
  (match method
    [`(get)
     (extend-hintdb
      common-hintdb
      [maybe-merge-after-recv (maybe-merge-after-recv #x1a0)])]
    [`(add ,x)
     (extend-hintdb
      common-hintdb
      [maybe-merge-after-recv (maybe-merge-after-recv #xec)])]))

(define (maybe-merge-after-recv instr-addr)
  (tactic
   (define ckt (lens-view (lens 'interpreter 'globals 'circuit) (get-state)))
   (void)
   #;(when (and (equal? (get-field ckt 'wrapper.soc.cpu.reg_pc) (bv instr-addr 32)) (equal? (get-field ckt 'wrapper.soc.cpu.cpu_state) (bv #x20 8)))
     (eprintf "pc: ~v state: ~v, merging~n" (get-field ckt 'wrapper.soc.cpu.reg_pc) (get-field ckt 'wrapper.soc.cpu.cpu_state))
     (merge!))))

(define common-hintdb
  (make-hintdb
   [concretize-boot
    (tactic
     (concretize! (lens 'circuit (field-filter/or "pwrmgr_state" "rom")) #:use-pc #t))]
   [concretize-uart
    (concretize! (lens 'circuit (field-filter/or 'wrapper.soc.uart.simpleuart.recv_buf_data 'wrapper.soc.uart.simpleuart.recv_pattern)))]
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
