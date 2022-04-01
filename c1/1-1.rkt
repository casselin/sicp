#lang sicp
;;; Chapter 1.1

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; Exercise 1.1
#|
10
> 10

(+ 5 3 4)
> 12

(- 9 1)
> 8

(/ 6 2)
> 3

(+ (* 2 4) (- 4 6))
> 6

(define a 3)
(define b (+ a 1))

(+ a b (* a b))
> 19

(= a b)
> #f

(if (and (> b a) (< b (* a b)))
    b
    a)
> 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
> 16

(+ 2 (if (> b a) b a))
> 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
> 16
|#

;; Exercise 1.2
(/ (+ 5 4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;; Exercise 1.3
(define (sum-square-largest x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((< y z) (sum-of-squares x z))
        (else (sum-of-squares x y))))

;; Exercise 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
#|
Changes the operator to ensure that we are adding (abs b) to a
|#

;; Exercise 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
#|
Applicative order evaluation:
(test 0 (p))
will loop infinitely as it recursively evaluates (p)

Normal order evaluation:
(test 0 (p))
= (if (= 0 0)
      0
      (p))
= 0

In normal order evaluation (p) will only be evaluated
if the predicate evaluates to false, which in (test 0 (p))
the predicate is true.
|#


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Exercise 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter2 guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter2 (improve guess x)
                      x)))

#|
Using sqrt-iter2 to compute square roots will result in an infinite loop.
This is because, using applicative order evaluation, we must evaluate
the else-clause sqrt-iter2 before we can apply new-if. This procedure
will never evaluate the predicate, thus can never terminate.
|#

;; Exercise 1.7
#|
good-enough? will not be accurate because squaring a very small number
will create a much smaller number, far below the limits on precision
that we can set (currently 0.001)
Likewise, squaring a large number will exceed the limits of memory,
rendering the procedure ineffective.
|#
(define (sqrt2 x)
  (define (sqrt-iter3 guess prev)
  (if (good-enough2? guess prev)
      guess
      (sqrt-iter3 (improve guess) guess)))

  (define (good-enough2? guess prev)
    (< (abs (- guess prev)) 0.001))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (average a b)
    (/ (+ a b) 2))

  (sqrt-iter3 1.0 x))

;; Exercise 1.8
(define (cbrt x)
  (define (cbrt-iter guess prev)
    (if (good-enough2? guess prev)
        guess
        (cbrt-iter (improve guess) guess)))

  (define (good-enough2? guess prev)
    (< (abs (- guess prev)) 0.001))

  (define (improve guess)
    (avg (/ x (square guess)) guess guess))

  (define (avg a b c)
    (/ (+ a b c) 3))

  (cbrt-iter 1.0 x))

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