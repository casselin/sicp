#lang sicp
;;; Section 2.1

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; replaced by exercise 2.1
;(define (make-rat n d)
;  (let ((g (gcd n d)))
;    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Exercise 2.1
(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (if (< d 0)
        (cons (* -1 (/ n g)) (/ (abs d) g))
        (cons (/ n g) (/ d g)))))