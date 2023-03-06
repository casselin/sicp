#lang sicp
;;; Section 3.5 Streams

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      (begin (newline)
             'done)
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;; Exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (take-n-stream n stream)
  (cond ((stream-null? stream) '())
        ((= n 0) '())
        (else
         (cons (stream-car stream)
               (take-n-stream (- n 1) (stream-cdr stream))))))

;; Exercise 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))

;; Exercise 3.55
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (stream-cdr stream)
                            (partial-sums stream))))

;; Exercise 3.56
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))
(define S (cons-stream 1 (merge (merge (scale-stream S 2)
                                       (scale-stream S 3))
                                (scale-stream S 5))))

;; Exercise 3.59
; a
(define (integrate-series series)
  (define (iter stream n)
    (cons-stream (* (/ 1 n) (stream-car stream))
                 (iter (stream-cdr stream) (+ n 1))))
  (iter series 1))

; b
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; Exercise 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (mul-series (stream-cdr s1) s2)
                            (scale-stream (stream-cdr s2) (stream-car s1)))))

;; Exercise 3.61
(define (invert-unit-series s)
  (cons-stream 1 (mul-series (scale-stream (stream-cdr s) -1)
                             (invert-unit-series s))))

;; Exercise 3.62
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "Denominator series cannot have zero constant term -- DIV-SERIES")
      (mul-series s1
                  (scale-stream (invert-unit-series s2) (/ 1 (stream-car s2))))))