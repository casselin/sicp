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

;; Exercise 1.14
#|
The tree for (count-change 11 5) is as follows:

`-- (cc 11 5)
    |-- (cc 11 4)
    |   |-- (cc 11 3)
    |   |   |-- (cc 11 2)
    |   |   |   |-- (cc 11 1)
    |   |   |   |   |-- (cc 11 0)
    |   |   |   |   `-- (cc 10 1)
    |   |   |   |       `-- ...
    |   |   |   |           |-- (cc 1 0)
    |   |   |   |           `-- (cc 0 1)
    |   |   |   `-- (cc 6 2)
    |   |   |       |-- (cc 6 1)
    |   |   |       |   `-- ...
    |   |   |       |       |-- (cc 1 0)
    |   |   |       |       `-- (cc 0 1)
    |   |   |       `-- (cc 1 2)
    |   |   |           |-- (cc 1 1)
    |   |   |           |   |-- (cc 1 0)
    |   |   |           |   `-- (cc 0 1)
    |   |   |           `-- (cc -4 2)
    |   |   `-- (cc 1 3)
    |   |       |-- (cc 1 2)
    |   |       |   |-- (cc 1 1)
    |   |       |   |   |-- (cc 1 0)
    |   |       |   |   `-- (cc 0 1)
    |   |       |   `-- (cc -4 2)
    |   |       `-- (cc -9 3)
    |   `-- (cc -14 4)
    `-- (cc -39 5)

The amount of space consumed by this process is the maximum height of the tree.
The maximum height will be the path that decreases the denomination of the
coins to the minimum denomination before reducing the change total. The length
of this path increases linearly with the amount to be changed.

Observe that for an amount to change n, there will be ceil(n/50) calls of the
form (cc x 5). Each of these calls will generate a subtree of the form (cc x 4).
Each subtree of this form will have ceil(y/25) calls of the form (cc y 4).
Each of these calls will generate a subtree of the form (cc y 3), and so on.
Following this pattern, we obtain (ignoring the ceiling function):
(n/50)*(n/25)*(n/10)*(n/5)*(n) = \Theta(n^5)
Thus the number of steps increases quintically with the amount of change.
|#

;; Exercise 1.15
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

#|
a.
12.15/3^n <= 0.1
121.5 <= 3^n
log_3(121.5) <= n
4.3691 <= n

p will be applied 4 times when (sine 12.15) is evaluated (p is not applied
once the angle is less than or equal to 0.1)

b.
The space and number of steps increases logarithmically with the size of the
angle
|#

;; Exercise 1.16
(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (* b b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

;; Exercise 1.17
(define (fast-mult a n)
  (cond ((= n 0) 0)
        ((even? n) (double (fast-mult a (halve n))))
        (else (+ a (fast-mult a (- n 1))))))

(define (double n)
  (+ n n))

; To be used for even numbers only
(define (halve n)
  (/ n 2))

;; Exercise 1.18
(define (fast-mult-iter b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (double b) (halve n)))
          (else (iter (+ a b) b (- n 1)))))
  (iter 0 b n))

;; Exercise 1.19
#|
T_{pq} = a <- bq + aq + ap
         b <- bp + aq

T^2_{pq} = a <- (bp+aq)q + (bq+aq+ap)q + (bq+aq+ap)p
             =  b(2pq+q^2) + a(2pq+q^2) + a(p^2+q^2)

           b <- (bp+aq)p + (bq+aq+ap)q
             =  b(p^2+q^2) + a(2pq+q^2)

Thus we see that p' = p^2 + q^2
                 q' = 2pq + q^2
|#
(define (fib n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (iter a
                 b
                 (+ (* p p) (* q q))
                 (+ (* 2 p q) (* q q))
                 (/ count 2)))
          (else (iter (+ (* b q) (* a q) (* a p))
                      (+ (* b p) (* a q))
                      p
                      q
                      (- count 1)))))
  (iter 1 0 0 1 n))

;; Exercise 1.20
#|
(rem* a b) indicates a remainder operation that will be performed
Normal-order evaluation of (gcd 206 40)
(gcd 206 40)
(gcd 40 (rem* 206 40))
(gcd (rem 206 40) (rem* 40 (rem* 206 40)))
(gcd (rem 40 (rem 206 40)) (rem* (rem* 206 40) (rem* 40 (rem* 206 40))))
(gcd (rem (rem 206 40) (rem 40 (rem 206 40)))
          (rem* (rem* 40 (rem* 206 40))
                (rem* (rem* 206 40) (rem* 40 (rem* 206 40)))))
(rem* (rem* 206 40) (rem* 40 (rem* 206 40)))
2
For a total of 18 performed operations of remainder

Applicative-order evaluation of (gcd 206 40)
(gcd 206 40)
(gcd 40 (rem* 206 40))
(gcd 40 6)
(gcd 6 (rem* 40 6))
(gcd 6 4)
(gcd 4 (rem* 6 4))
(gcd 4 2)
(gcd 2 (rem* 4 2))
(gcd 2 0)
2
For a total of 4 performed operations of remainder

|#