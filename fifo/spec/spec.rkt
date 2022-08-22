#lang knox/spec

#:init s0
#:symbolic-constructor new-symbolic-state
#:methods
(full)
(empty)
(push [v (bitvector WIDTH)])
(peek)
(pop)

(require rosutil)

(provide WIDTH CAPACITY-LOG2 CAPACITY)

(define WIDTH 32)
(define CAPACITY-LOG2 2)
(define CAPACITY (sub1 (expt 2 CAPACITY-LOG2)))

(define (new-symbolic-state)
  (choose
   (let rec ([l (symbolic-list-of-length CAPACITY)])
     (if (empty? l)
         (list (list))
         (cons l (rec (cdr l)))))))

(define s0 (list))

(define ((full) s)
  (result (equal? (length s) CAPACITY) s))

(define ((empty) s)
  (result (empty? s) s))

(define ((push v) s)
  (result (void)
          (if (equal? (length s) CAPACITY)
              s
              (append s (list v)))))

(define ((peek) s)
  (result (if (empty? s) #f (first s)) s))

(define ((pop) s)
  (result (void) (if (empty? s) s (rest s))))

(define (symbolic-list-of-length n)
  (if (zero? n)
      (list)
      (cons (fresh-symbolic 'value (bitvector WIDTH)) (symbolic-list-of-length (sub1 n)))))

(define (choose ls)
  (if (equal? (length ls) 1)
      (first ls)
      (let-values ([(l r) (split-at ls (quotient (length ls) 2))])
        (if (fresh-symbolic 'choose boolean?)
            (choose l)
            (choose r)))))
