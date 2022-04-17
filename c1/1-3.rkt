#lang sicp
(#%require "1-2.rkt")
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