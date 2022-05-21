#lang sicp
;; Section 2.1.4 - Interval Arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

#| Replaced by Exercise 2.11
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
|#

;; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound z) (car z))
(define (upper-bound z) (cdr z))

;; Exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; Exercise 2.9
(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

#|
Addition of intervals
(width (add-interval (make-interval a b) (make-interval c d)))
(width (make-interval (+ a c) (+ b d)))
(/ (- (+ b d) (+ a c)) 2)
(+ (/ (- b a) 2) (/ (- d c) 2))
(+ (width (make-interval a b)) (width (make-interval c d)))

Subtraction of intervals
(width (sub-interval (make-interval a b) (make-interval c d)))
(width (make-interval (- a d) (- b c)))
(/ (- (- b c) (- a d)) 2)
(/ (+ (- b a) (- d c)) 2)
(+ (/ (- b a) 2) (- d c) 2)
(+ (width (make-interval a b) (make-interval c d)))

If the width of a product of two intervals depends only on the width of these
intervals, then the width of any product of two intervals with width 1 should
be the same. But,
(width (mul-interval (make-interval 3 5) (make-interval 1 3)))
(width (make-interval (3 15)))
6

(width (mul-interval (make-interval -2 0) (make-interval 0 2)))
(width (make-interval -4 0))
2

are not equal. Thus the width of a product is not a function of the widths of
the factors
|#

;; Exercise 2.10
(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0))
      (error "Cannot divide by an interval that spans zero")
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

;; Exercise 2.11
(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((and (>= xl 0)
                (>= xu 0)
                (>= yl 0)
                (>= yu 0))
           (make-interval (* xl yl) (* xu yu)))
          ((and (<= xl 0)
                (>= xu 0)
                (>= yl 0)
                (>= yu 0))
           (make-interval (* xl yu) (* xu yu)))
          ((and (>= xl 0)
                (>= xu 0)
                (<= yl 0)
                (>= yu 0))
           (make-interval (* xu yl) (* xu yu)))
          ((and (<= xl 0)
                (>= xu 0)
                (<= yl 0)
                (>= yu 0))
           (make-interval (min (* xu yl) (* xl yu))
                          (* xu yu)))
          ((and (<= xl 0)
                (<= xu 0)
                (>= yl 0)
                (>= yu 0))
           (make-interval (* xl yu) (* xu yl)))
          ((and (>= xl 0)
                (>= xu 0)
                (<= yl 0)
                (<= yu 0))
           (make-interval (* xu yl) (* xl yu)))
          ((and (<= xl 0)
                (<= xu 0)
                (<= yl 0)
                (>= yu 0))
           (make-interval (* xl yu) (* xl yl)))
          ((and (<= xl 0)
                (>= xu 0)
                (<= yl 0)
                (<= yu 0))
           (make-interval (* xu yl) (* xl yu)))
          ((and (<= xl 0)
                (<= xu 0)
                (<= yl 0)
                (<= yu 0))
           (make-interval (* xu yu) (* xl yl))))))

;; Exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (make-center-percent c p)
  (let ((lb (- c (* c p)))
        (ub (+ c (* c p))))
    (make-interval lb ub)))

(define (percent i)
  (/ (width i) (center i)))

;; Exercise 2.13
#|
The percentage tolerance of a product of intervals
(make-center-percent c1 p1)
(make-center-percent c2 p2) where c1,c2 are positive, p1,p2 are small is

((c1+p1c1)(c2p2c2)-(c1-p1c1)(c2-p2c2))/(c1+p1c1)(c2+p2c2)+(c1-p1c1)(c2-p2c2)
= (p1 + p2) / (1 + p1*p2)

If p1 and p2 are sufficiently small, then p1*p2 ~ 0 and the result above is
approximated by p1 + p2
|#

;; Exercise 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
(define A (make-center-percent 6.8 0.1))
(define B (make-center-percent 4.7 0.05))
(define A-over-A (div-interval A A))
(define A-minus-A (sub-interval A A))
(define A-over-B (div-interval A B))
#|
> (par1 A B)
(2.201031010873943 . 3.4873689182805854)
> (par2 A B)
(2.581558809636278 . 2.97332259363673)
we see that Lem is correct since these are not the same intervals

> (center A-over-A)
1.02020202020202
> (percent A-over-A)
0.19801980198019795

> (center A-over-B)
1.4576867701167813
> (percent A-over-B)
0.1492537313432836

> A-minus-A
(-1.3599999999999994 . 1.3599999999999994)
> A-over-A
(0.8181818181818182 . 1.222222222222222)

Here we see that what the additive and multiplicative inverses
of an interval are not what we expect. This explains why the
algebraically equivalent expressions are not equivalent when
dealing with intervals.
|#

;; Exercise 2.15
#|
Eva is correct because each time an uncertain variable is used in an
operation, additional error/uncertainty is introduced. Thus to produce
the tightest error bounds, we must be careful to introduce the least
amount of uncertainty. This is accomplished by minimizing the number
of appearances of an uncertain variable in a formula.
|#

;; Exercise 2.16
#|
Equivalent algebraic expressions may produce different results for the
reason outlined in Exercise 2.15. In addition, the operations for interval
arithmetic do not form a field. This means we cannot manipulate expressions
involving intervals as we could with expressions involving (real) numbers.
As such, a package without this defect would need to partition expressions
into some equivalence class, and then convert any given expression into the
representative for that class before performing the arithmetic.
|#