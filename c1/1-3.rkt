#lang sicp
;;; Chapter 1.3
; Needed from Section 1.2
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; From Section 1.3

(define (cube x) (* x x x))
(define (identity x) x)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (define (avg a b)
    (/ (+ a b) 2))
  (lambda (x) (avg x (f x))))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; Exercise 1.29
; n must be even
(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (next x) (+ x (* 2 h)))
  (* (/ h 3.0)
     (+ (f a)
        (* 4 (sum f (+ a h) next (- b h)))
        (* 2 (sum f (+ a (* 2 h)) next (- b (* 2 h))))
        (f b))))
#|
> (simpsons-rule cube 0 1 100)
0.25

> (simpsons-rule cube 0 1 1000)
0.25
|#

;; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.31
; a)
(define (product factor a next b)
  (if (> a b)
      1
      (* (factor a)
         (product factor (next a) next b))))

(define (fact n)
  (if (< n 1)
      1
      (product identity 1 inc n)))

(define (pi-prod a b)
  (define (pi-factor x)
    (/ (* (- x 1) (+ x 1))
       (square x)))
  (define (pi-next x) (+ x 2))
  (if (= a 1.0)
      (product pi-factor 3.0 pi-next b)
      (product pi-factor a pi-next b)))

(define (pi-approx n)
  (* 4 (pi-prod 1 n)))

;b)
(define (product-iter factor a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (factor a) result))))
  (iter a 1))

;; Exercise 1.32
; a)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (prod-acc factor a next b)
  (accumulate * 1 factor a next b))

; b)
(define (accum-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner (term a) result))))
  (iter a null-value))

;; Exercise 1.33
(define (filtered-accumulate pred combiner null-value term a next b)
  (define (filtered-term x)
    (if (pred x)
        (term x)
        null-value))
  (accumulate combiner null-value filtered-term a next b))

; a) needs prime? from Section 1.2
(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

; b) needs GCD from Section 1.2
(define (coprime-product n)
  (define (coprime? x)
    (= (gcd n x) 1))
  (filtered-accumulate coprime? * 1 identity 1 inc (- n 1)))

;; Exercise 1.34
#|
> (f f)
= (f 2)
= (2 2)

The interpreter would attempt to evaluate the procedure 2 and fail
because it is not a procedure.
|#

;; Exercise 1.35
#|
x = 1 + 1/x
x^2 = x + 1
x^2 - x - 1 = 0

With \phi being a root of this polynomial. Thus \phi is a fixed point of
x -> 1 + 1/x
|#
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

;; Exercise 1.36
(define (fixed-point2 f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (cond ((close-enough? guess next)
             (newline)
             next)
            (else
             (newline)
             (display next)
             (try next)))))
  (try first-guess))

(define (x-to-the-x)
  (fixed-point2 (lambda (x) (/ (log 1000) (log x)))
                2.0))

(define (x-to-the-x-damping)
  (define (average x y)
    (/ (+ x y) 2))
  (fixed-point2 (lambda (x) (average x (/ (log 1000) (log x))))
                2.0))
#|
Without average damping takes 34 steps.
With average damping takes 9 steps.
|#

;; Exercise 1.37
; a)
(define (cont-frac1 n d k)
  (define (recurse i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i) (recurse (+ i 1))))))
  (recurse 1))
#|
k = 11 produces a result accurate to 4 decimal places.
|#
; b)
(define (cont-frac2 n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i) result)))))
  (iter k 0))

;; Exercise 1.38
(define (nat-exp k)
  (+ 2
     (cont-frac1 (lambda (i) 1.0)
                 (lambda (i)
                   (if (= (remainder i 3) 2)
                       (* 2 (/ (+ i 1) 3))
                       1))
                 k)))

;; Exercise 1.39
(define (tan-cf x k)
  (cont-frac1 (lambda (i)
                (if (= i 1)
                    x
                    (* -1 (square x))))
              (lambda (i) (- (* 2 i) 1.0))
              k))

;; Exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;; Exercise 1.41
(define (double g)
  (lambda (x)
    (g (g x))))
#|
> (((double (double double)) inc) 5)
= (((double (lamdba (f) (double (double f)))) inc) 5)
= (((lambda (f) (double (double (double (double f))))) inc) 5)
= ((double (double (double (double inc)))) 5)
= 21
|#

;; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;; Exercise 1.43
(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

;; Exercise 1.44
(define (smooth f)
  (define (average a b c)
    (/ (+ a b c) 3))
  (lambda (x)
    (average (f (- x dx))
             (f x)
             (f (+ x dx)))))

; The n-fold smoothed function is given by
(define (n-fold-smoothed f n)
  ((repeated smooth n) f))

;; Exercise 1.45
(define (expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (* b b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root x n)
  (let ((damped-g
         ((repeated average-damp (floor (log2 n)))
          (lambda (y) (/ x (expt y (- n 1)))))))
    (fixed-point damped-g 1.0)))

;; Exercise 1.46
(define (iterative-improve good-enough? improve)
  (define (go guess)
    (if (good-enough? guess)
        guess
        (go (improve guess))))
  (lambda (guess) (go guess)))

(define (sqrt-ii x)
  ((iterative-improve (lambda (guess)
                       (< (abs (- (square guess) x)) 0.00001))
                      (lambda (guess)
                       (/ (+ guess (/ x guess)) 2)))
   1.0))

(define (fixed-point-ii f first-guess)
  ((iterative-improve (lambda (guess)
                        (< (abs (- guess (f guess))) 0.00001))
                      (lambda (guess)
                        (f guess)))
   first-guess))
#|
This does not have the same behaviour as the fixed-point procedure
from 1.3.4. Specifically, once the guess is good enough it returns
the first guess rather than the next guess. Whereas the procedure
from 1.3.4 returns the next guess instead. I believe this is not
practically important, since once the guess is "good enough," both
this guess and the next guess are within the given tolerance.
|#
