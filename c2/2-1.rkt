#lang sicp
;;; Section 2.1
(define (square x) (* x x))

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

;; Exercise 2.2
(define (make-segment start end)
  (cons start end))

(define (start-point segment)
  (car segment))

(define (end-point segment)
  (cdr segment))

(define (midpoint-segment segment)
  (define (average a b)
    (/ (+ a b) 2))
  (let ((x-start (x-point (start-point segment)))
        (y-start (y-point (start-point segment)))
        (x-end (x-point (end-point segment)))
        (y-end (y-point (end-point segment))))
    (make-point (average x-start x-end)
                (average y-start y-end))))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Exercise 2.3
(define (peri-rect r)
  (* 2 (+ (length-rect r)
          (width-rect r))))

(define (area-rect r)
  (* (length-rect r)
     (width-rect r)))

; Representation using a length and width line segment
(define (make-rect length-segment width-segment)
  (cond ((not (equal-endpoints? length-segment width-segment))
         (error "length and width segments do not meet at endpoints"))
        ((not (orthogonal-segments? length-segment width-segment))
         (error "length and width segments not orthogonal"))
        (else (cons length-segment width-segment))))

(define (equal-endpoints? seg1 seg2)
  (or (equal-points? (start-point seg1) (start-point seg2))
      (equal-points? (start-point seg1) (end-point seg2))
      (equal-points? (end-point seg1) (start-point seg2))
      (equal-points? (end-point seg1) (end-point seg2))))

(define (equal-points? pt1 pt2)
  (and (= (x-point pt1) (x-point pt2))
       (= (y-point pt1) (y-point pt2))))

(define (orthogonal-segments? seg1 seg2)
  (let ((vec1 (seg-to-vec seg1))
        (vec2 (seg-to-vec seg2)))
    (= (dot-product vec1 vec2) 0)))

(define (seg-to-vec segment)
  (let ((x (- (x-point (end-point segment))
              (x-point (start-point segment))))
        (y (- (y-point (end-point segment))
              (y-point (start-point segment)))))
    (make-point x y)))

(define (dot-product a b)
  (+ (* (x-point a) (x-point b))
     (* (y-point a) (y-point b))))

(define (length-rect r)
  (distance (car r)))

(define (width-rect r)
  (distance (cdr r)))

(define (distance segment)
  (sqrt (+ (square (- (x-point (end-point segment))
                      (x-point (start-point segment))))
           (square (- (y-point (end-point segment))
                      (y-point (start-point segment)))))))

; Representation using a side and a diagonal
; Perimeter and Area procedures are the same, simply renamed to avoid
; conflict with previous implementation
(define (peri-rect2 r)
  (* 2 (+ (length-rect2 r)
          (width-rect2 r))))

(define (area-rect2 r)
  (* (length-rect2 r)
     (width-rect2 r)))

(define (make-rect2 length-seg width-seg)
  (cond ((not (equal-endpoints? length-seg width-seg))
         (error "length and width segments do not meet at endpoints"))
        ((not (orthogonal-segments? length-seg width-seg))
         (error "length and width segments not orthogonal"))
        (else (let ((diag (cond
                            ((equal-points? (start-point length-seg) (start-point width-seg))
                             (make-segment (end-point length-seg) (end-point width-seg)))
                            ((equal-points? (start-point length-seg) (end-point width-seg))
                             (make-segment (end-point length-seg) (start-point width-seg)))
                            ((equal-points? (end-point length-seg) (start-point width-seg))
                             (make-segment (start-point length-seg) (end-point width-seg)))
                            (else
                             (make-segment (start-point length-seg) (start-point width-seg))))))
                (cons length-seg diag)))))
                  

(define (length-rect2 r)
  (distance (car r)))

; Use Pythagoras' Thereom to determine length of width
(define (width-rect2 r)
  (let ((a (distance (car r)))
        (c (distance (cdr r))))
    (sqrt (- (square c) (square a)))))

;; Exercise 2.4
#|
(define (cdr z)
  (lambda (p q) q))

> (car (cons x y))
= (car (lambda (m) (m x y)))
= ((lambda (m) (m x y)) (lambda (p q) p))
= ((lambda (p q) p) x y)
= x

> (cdr (cons x y))
= (cdr (lambda (m) (m x y)))
= ((lambda (m) (m x y)) (lambda (p q) q))
= ((lambda (p q) q) x y)
= y
|#

;; Exercise 2.5
; Using make-pair, fst, snd instead of cons, car, cdr
(define (make-pair a b)
  (* (expt 2 a) (expt 3 b)))
(define (fst z)
  (define (iter count n)
    (if (even? n)
        (iter (+ count 1) (/ n 2))
        count))
  (iter 0 z))
(define (snd z)
  (define (iter count n)
    (if (= (remainder n 3) 0)
        (iter (+ count 1) (/ n 3))
        count))
  (iter 0 z))

;; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

#|
(add-1 zero)
(add-1 (lambda (f) (lambda (x) x)))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))

(add-1 one)
(add-1 (lambda (f) (lambda (x) (f x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))
|#
(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))