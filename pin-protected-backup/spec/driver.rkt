#lang knox/driver

#:idle [cts #t rx #t]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(define BAUD-RATE 6)
(define BAUD-RATE-DIV2 (quotient BAUD-RATE 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions

(define (tick-n n)
  (if (zero? n)
      (void)
      (begin
        (tick)
        (hint maybe-concretize-cpu)
        (hint maybe-split-pc)
        (hint debug)
        (tick-n (sub1 n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UART

(define (read-bit)
  (let ([v (output-tx (in))])
    ;; tick for BAUD-RATE cycles
    (tick-n BAUD-RATE)
    v))

(define (wait-until-start-bit)
  (hint maybe-merge-after-recv)
  (if (output-tx (in))
      (begin
        (tick)
        (hint maybe-concretize-cpu)
        (hint maybe-split-pc)
        (hint debug)
        (wait-until-start-bit))
      (void)))

(define (recv-byte)
  ;; say that we are ready to receive
  (out* 'cts #f)
  ;; wait until we see the start bit
  (wait-until-start-bit)
  (hint overapproximate-recv-merge)
  (hint merge)
  ;; no longer ready to receive
  (out* 'cts #t)
  ;;advance to middle of the start bit
  (tick-n BAUD-RATE-DIV2)
  ;; read start bit, discard result
  (read-bit)
  ;; read the bits
  (let ([res (apply concat (map bool->bitvector (reverse (collect read-bit 8))))])
    ;; read stop bit, discard result
    (read-bit)
    res))

(define (recv-bytes n)
  (collect
   (lambda ()
     (hint overapproximate-uart)
     (yield uart-fp)
     (recv-byte))
   n))

(define (send-bit bit)
  (out* 'rx bit)
  (tick-n BAUD-RATE))

(define (wait-until-clear-to-send)
  (if (output-rts (in))
      (begin
        (tick)
        (hint maybe-concretize-cpu)
        (hint debug)
        (wait-until-clear-to-send))
      (void)))

(define (for-range low high proc)
  (if (equal? low high)
      (void)
      (begin
        (proc low)
        (for-range (add1 low) high proc))))

(define (send-byte byte)
  ;; wait until device is ready
  (wait-until-clear-to-send)
  (hint overapproximate-send-merge)
  (hint merge)
  ;; send start bit
  (send-bit #f)
  ;; send data bits
  (for-range 0 8 (lambda (i) (send-bit (bitvector->bool (extract i i byte)))))
  ;; not sure if this is necessary, but it's cheap:
  (hint concretize-uart)
  ;; send stop bit
  (send-bit #t))

(define (send-bytes bytes)
  (if (null? bytes)
      (void)
      (begin
        (hint overapproximate-uart)
        (yield uart-fp)
        (send-byte (car bytes))
        (send-bytes (cdr bytes)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main driver

(define (reset)
  (out* 'rx #t 'cts #f)
  (tick)
  (hint debug)
  (out* 'cts #t)
  (tick-n 220)
  (hint overapproximate-boot)) ; do this after UART has been initialized

(define (prepare cmd slot)
  (reset)
  (hint overapproximate-uart)
  (yield uart-fp)
  (send-byte cmd)
  (hint overapproximate-uart)
  (yield uart-fp)
  (hint merge)
  (hint case-split-slot)
  (send-byte slot))

(define (wait-and-return value)
  (tick-n 100) ; to give circuit time to power off
  value)

(define (status slot)
  (let ([invalid-slot (bvuge slot (bv 4 8))])
    (hint case-split-invalid-slot)
    (if invalid-slot
        #f
        (begin
          (prepare (bv 1 8) slot)
          (wait-and-return (bveq (recv-byte) (bv 1 8)))))))

(define (delete slot)
  (let ([invalid-slot (bvuge slot (bv 4 8))])
    (hint case-split-invalid-slot)
    (if invalid-slot
        #f
        (begin
          (prepare (bv 2 8) slot)
          (wait-and-return (bveq (recv-byte) (bv 1 8)))))))

(define (store slot pin data)
  (let ([invalid-slot (bvuge slot (bv 4 8))])
    (hint case-split-invalid-slot)
    (if invalid-slot
        #f
        (begin
          (prepare (bv 3 8) slot)
          (send-bytes (bitvector->bytes pin))
          (send-bytes (bitvector->bytes data))
          (wait-and-return (bveq (recv-byte) (bv 1 8)))))))

(define (retrieve slot pin)
  (let ([invalid-slot (bvuge slot (bv 4 8))])
    (hint case-split-invalid-slot)
    (if invalid-slot
        #f
        (begin
          (prepare (bv 4 8) slot)
          (send-bytes (bitvector->bytes pin))
          (wait-and-return
           (if (bveq (recv-byte) (bv 3 8))
               (apply concat (recv-bytes 16))
               #f))))))
