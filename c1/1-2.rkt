#lang sicp
;;; Chapter 1.2

;; Exercise 1.9
#|
First procedure
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

This is a recursive process

Second procedure
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

This is an iterative process
|#

;; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

#|
(A 1 10)
1024

(A 2 4)
65536

(A 3 3)
65536
|#

(define (f n) (A 0 n))
#|
f(n) = 2*n
|#

(define (g n) (A 1 n))
#|
(A 1 n)
(A 0 (A 1 (- n 1)))
(* 2 (A 0 (A 1 (- n 1))))
(* 2 (* 2 (* 2....2)))

Thus g(n) = 2^n
|#

(define (h n) (A 2 n))
#|
(h 2)
(A 2 n)
(A 1 (A 2 (- n 1)))
(A 1 (h (- n 1)))        let m = (h (- n 1))
(A 0 (A 1 (- m 1)))
(* 2 (A 0 (A 1 (- m 2))))
(* 2 (* 2 (A 1 (- m 2))))
There will be m = (h (- n 1)) factors of 2 in the product
Therefore (h n) = 2^(h (- n 1))
|#