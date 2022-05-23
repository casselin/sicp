#lang sicp
;;; Section 2.2

;; Exercise 2.17
(define (last-pair xs)
  (if (null? (cdr xs))
      (list (car xs))
      (last-pair (cdr xs))))

;; Exercise 2.18
(define (reverse xs)
  (define (iter ret ys)
    (if (null? ys)
        ret
        (iter (cons (car ys) ret) (cdr ys))))
  (iter nil xs))

;; Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))
#|
The order of the list of coins does not impact the result because
the algorithm considers all possible combinations of coins.
|#

;; Exercise 2.20
(define (same-parity x . y)
  (define (helper parity y)
    (cond ((null? y) nil)
          ((= parity (remainder (car y) 2))
           (cons (car y) (helper parity (cdr y))))
          (else
           (helper parity (cdr y)))))
  (cons x (helper (remainder x 2) y)))

;; Exercise 2.21
(define (square x) (* x x))
(define (square-list1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map square items))

;; Exercise 2.22
#|
This iterative implementation produces the answer in the reverse order
because the first item in the input list is placed in the answer list
first. This makes it the final item in the answer list. Then the second
item in the input list is the second last, and so on. This produces an
answer list in the reverse order.

The second iterative implementation does not produce the correct answer
because to construct a single list using cons, the second argument
must be a list. Instead, the implementation produces a sequence of pairs
where the `car` of the pair is the "previous" answer and the `cdr` of the
pair is the "current" answer.
|#

;; Exercise 2.23
(define (for-each f items)
  (cond ((null? items) true)
        (else
         (f (car items))
         (for-each f (cdr items)))))