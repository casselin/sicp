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

;; Exercise 3.64
(define (stream-limit stream tolerance)
  (let ((first (stream-ref stream 0))
        (second (stream-ref stream 1)))
    (if (< (abs (- first second)) tolerance)
        second
        (stream-limit (stream-cdr stream) tolerance))))

;; Exercise 3.65
; slowest convergence
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))
; eight terms bound value of ln2 between 0.7595 and 0.6345

; accelerated convergence
(define (square x) (* x x))
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))
; eight terms bound value of ln2 between 0.6933 and 0.6930

; "super-accelerated" convergence
(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
; eight terms bound value of ln2 between 0.693147180560 and 0.693147180559

;; Exercise 3.66
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

; testing
(define int-pairs (pairs integers integers))

(define (row-1-spec? i)
    (equal? (stream-ref int-pairs (row-1-pos i)) (list 1 i)))

(define (row-1-pos i)
    (if (= i 1)
        0
        (- (* 2 (- i 1)) 1)))

(define (test-row-1 n)
    (define (iter i)
      (if (> i n)
          'done!
          (if (row-1-spec? i)
              (iter (+ i 1))
              (error "Specification failed for " i))))
  (iter 1))

(define (row-i-spec? i j)
  (and (not (> i j))
       (equal? (stream-ref int-pairs (row-i-pos i j)) (list i j))))

(define (row-i-pos i j)
    (define (up-row k new-pos)
      (if (= k 1)
          new-pos
          (up-row (- k 1) (* 2 (+ new-pos 1)))))
    (if (> i j)
        (error "Pair does not exist in int-pairs" (list i j))
        (up-row i (row-1-pos (+ (- j i) 1)))))

#|
The row-i-pos procedure produces the index corresponding to the pair (i, j)
in the stream (pairs integers integers).

The logic behind this is to first consider the substream consisting of all
pairs (x,y) where x,y >= j, and to find the index of (i,j) in this substream.
This is equivalent to finding the index of the pair (1,j-i+1) in the original
stream (all pairs of the form (1,n) occur at the odd indices, except (1,1)
which occurs at index 0). Call this index k. Then, to "raise" this index into
the substream containing all pairs of the form with first element >= i-1, by
computing the (k+1)th even number. Repeat this process until you have raised
the index into the original stream.

Thus, 196 pairs precede (1,100)

950737950171172051122527404029 pairs precede (99,100)

1267650600228229401496703205373 pairs precede (100,100)
|#

;; Exercise 3.67
(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (interleave
     (stream-map (lambda (y) (list y (stream-car t)))
                 (stream-cdr s))
     (all-pairs (stream-cdr s) (stream-cdr t))))))

;; Exercise 3.68
#|
This version of the pairs procedure will create an infinite loop. This is due
to the environment model of execution attempting to evaluate the arguments
to interleave in the body of pairs. Note that the second argument to
interleave is the recursive call (pairs (stream-cdr s) (stream-cdr t)).
Thus the procedure runs in an infinite loop attempting to (recursively)
evaluate the second argument to interleave. This does not occur with the
original definition of pairs because the recursive call occurs in the delayed
portion of cons-stream.
|#

;; Exercise 3.69
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (pairs t (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagorean-triples
  (stream-filter (lambda (t)
                   (let ((i (car t))
                         (j (cadr t))
                         (k (caddr t)))
                     (= (+ (square i) (square j)) (square k))))
                 (triples integers integers integers)))

;; Exercise 3.70
(define (merge-weighted weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((s1carw (weight s1car))
                 (s2carw (weight s2car)))
             (cond ((< s1carw s2carw)
                    (cons-stream s1car
                                 (merge-weighted weight (stream-cdr s1) s2)))
                   ((> s1carw s2carw)
                    (cons-stream s2car
                                 (merge-weighted weight s1 (stream-cdr s2))))
                   (else
                    (cons-stream s1car
                                 (merge-weighted weight
                                                 (stream-cdr s1)
                                                 s2)))))))))

(define (weighted-pairs weight s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted weight
                   (stream-map (lambda (x) (list (stream-car s) x))
                               (stream-cdr t))
                   (weighted-pairs weight
                                   (stream-cdr s)
                                   (stream-cdr t)))))

; a
(define sum-int-pairs
  (weighted-pairs (lambda (p) (+ (car p) (cadr p)))
                  integers integers))

; b
(define non-multiples
  (stream-filter (lambda (n)
                   (not (or (even? n)
                            (zero? (remainder n 3))
                            (zero? (remainder n 5)))))
                 integers))
(define non-multiple-pairs
  (weighted-pairs (lambda (p)
                    (let ((i (car p))
                          (j (cadr p)))
                      (+ (* 2 i)
                         (* 3 j)
                         (* 5 i j))))
                  non-multiples
                  non-multiples))

;; Exercise 3.71
(define (cube x) (* x x x))
(define (sum-cubes list)
    (apply + (map cube list)))
(define (ramanujan-numbers)
  (define sum-cube-stream
    (weighted-pairs sum-cubes integers integers))
  (define (iter s)
    (if (= (sum-cubes (stream-car s))
           (sum-cubes (stream-car (stream-cdr s))))
        (cons-stream (sum-cubes (stream-car s)) (iter (stream-cdr s)))
        (iter (stream-cdr s))))
  (iter sum-cube-stream))

#|
> (take-n-stream 6 (ramanujan-numbers))
(1729 4104 13832 20683 32832 39312)
|#

;; Exercise 3.72
(define (all pred list)
  (if (null? list)
      true
      (and (pred (car list))
           (all pred (cdr list)))))
(define (sum-squares list)
  (apply + (map square list)))
(define (triple-sum-squares)
  (define sum-square-stream
    (weighted-pairs sum-squares integers integers))
  (define (iter s)
    (let ((weight (sum-squares (stream-car s)))
          (candidates (take-n-stream 3 s)))
      (if (all (lambda (p) (= (sum-squares p) weight)) (cdr candidates))
          (cons-stream (cons weight candidates)
                       (iter (stream-cdr s)))
          (iter (stream-cdr s)))))
  (iter sum-square-stream))

;; Exercise 3.73
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (scale-stream (integral i v0 dt) (/ 1 C)))))

;; Exercise 3.74
(define (sign-change-detector current previous)
  (if (>= current 0)
      (if (< previous 0) 1 0)
      (if (>= previous 0) -1 0)))
(define (list->stream list)
  (if (null? list)
      the-empty-stream
      (cons-stream (car list)
                   (list->stream (cdr list)))))
(define sense-data
  (list->stream (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

;; Exercise 3.75
(define (make-zero-crossings input-stream last-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avg)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

;; Exercise 3.76
(define (smooth input-stream)
  (if (or (stream-null? input-stream)
          (stream-null? (stream-cdr input-stream)))
      the-empty-stream
      (let ((fst (stream-car input-stream))
            (snd (stream-car (stream-cdr input-stream))))
        (let ((avpt (/ (+ fst snd) 2)))
          (cons-stream avpt
                       (smooth (stream-cdr input-stream)))))))

(define (make-zero-crossings-modular input-stream)
  (define smoothed-stream (smooth input-stream))
  (stream-map sign-change-detector
              smoothed-stream
              (cons-stream 0 smoothed-stream)))

;; Exercise 3.77
(define (integral-delay delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral-delay (delay (stream-cdr integrand))
                                (+ (* dt (stream-car integrand))
                                   initial-value)
                                dt)))))

; Tweaked to work with Racket's implementation of scheme (ie. #lang sicp)
(define (solve f y0 dt)
  (define y (integral-delay (delay (force dy)) y0 dt))
  (define dy (delay (stream-map f y)))
  y)

;; Exercise 3.78
; Tweaked to work with Racket's implementation of scheme (ie. #lang sicp)
(define (solve-2nd a b y0 dy0 dt)
  (define y (integral-delay (delay dy) y0 dt))
  (define dy (integral-delay (delay (force ddy)) dy0 dt))
  (define ddy (delay (add-streams (scale-stream dy a)
                                  (scale-stream y b))))
  y)

;; Exercise 3.79
; Tweaked to work with Racket's implementation of scheme (ie. #lang sicp)
(define (solve-gen-2nd f y0 dy0 dt)
  (define y (integral-delay (delay dy) y0 dt))
  (define dy (integral-delay (delay (force ddy)) dy0 dt))
  (define ddy (delay (stream-map f dy y)))
  y)

;; Exercise 3.80
(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (integral-delay (delay (force dvc)) vc0 dt))
    (define il (integral-delay (delay (force dil)) il0 dt))
    (define dvc (delay (scale-stream il (/ -1 C))))
    (define dil (delay (add-streams (scale-stream il (/ (- R) L))
                                    (scale-stream vc (/ 1 L)))))
    (cons vc il)))