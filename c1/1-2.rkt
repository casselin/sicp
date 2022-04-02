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

;; Exercise 1.11
(define (f-rec n)
  (cond ((< n 3) n)
        (else (+ (f-rec (- n 1))
                 (* 2 (f-rec (- n 2)))
                 (* 3 (f-rec (- n 3)))))))

(define (f-iter n)
  (define (iter a b c count)
    (if (= count 0)
        c
        (iter (+ a (* 2 b) (* 3 c))
              a
              b
              (- count 1))))
  (iter 2 1 0 n))

;; Exercise 1.12
(define (pascal r c)
  (cond ((or (< r 0) (< c 0)) 0)
        ((= r 0) 1)
        ((or (= c 0) (= c r)) 1)
        (else (+ (pascal (- r 1) (- c 1))
                 (pascal (- r 1) c)))))

;; Exercise 1.13
#|
Let \phi = (1 + sqrt(5))/2 and \psi = (1 - sqrt(5))/2
Will show that Fib(n) = (\phi^n - \psi^n)/sqrt(5) by strong induction:
Base case:
Fib(0) = (1 - 1)/sqrt(5) = 0
Fib(1) = (1+sqrt(5)/2 - 1+sqrt(5)/2)/sqrt(5) = 1

Inductive case:
Fib(n+1) = Fib(n) + Fib(n-1)
         = (\phi^n - \psi^n)/sqrt(5) + (\phi^(n-1) - \psi^(n-1))/sqrt(5)
         = (\phi^(n-1)(\phi + 1) - \psi^(n-1)(\psi + 1))/sqrt(5)
         = (\phi^(n-1)(\phi^2) - \psi^(n-1)(\psi^2))/sqrt(5)
         = (\phi^(n+1) - \psi^(n+1))/sqrt(5)
Used the fact that \phi^2 = \phi + 1 and \psi^2 = \psi + 1

We have that \psi < 1, and as n -> \inf \psi^n -> 0. Thus \psi^n is
insignificant in the closed formula of Fib(n), and we may simply
compute \phi^n/sqrt(5) and round to the nearest integer.
|#