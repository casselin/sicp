#lang sicp
;; Section 2.1.4 - Interval Arithmetic

(define (add-inteval x y)
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