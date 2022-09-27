#lang sicp

;;; Section 2.5.2
;; Implementation of arithmetic system using a tower of types:
; integer -> rational -> real -> complex
(define type-tower '(integer rational real complex))

;; get/put definitions from 3.3.3
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; Coercion table from Section 2.5.2
(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

;; Definitions from 2.4
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

#|
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))
|#

;; Generic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))

(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))    
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (make-rational x y)))
  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer)
       (lambda (x) (= 0 x)))
  (put 'make 'integer
       (lambda (x) (if (integer? x)
                       (tag x)
                       (error "Non-integer value -- MAKE-INTEGER" x))))
  ;; exercise 2.83
  (define (integer->rational n)
    (make-rational (contents n) 1))
  (put-coercion 'integer 'rational integer->rational)

  ;; exercise 2.86
  (put 'sq-rt '(integer)
       (lambda (n)
         (let ((root (sqrt n)))
           (make-complex-from-real-imag (make-real (real-part root))
                                        (make-real (imag-part root))))))
  (put 'cosine '(integer)
       (lambda (n)
         (make-real (cos n))))
  (put 'sine '(integer)
       (lambda (n)
         (make-real (sin n))))
  (put 'arctan '(integer integer)
       (lambda (n m)
         (make-real (atan n m))))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (and (integer? n) (integer? d))
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))
        (error "Non-integer numerator or denominator -- MAKE-RAT"
               (list n d))))
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
              (* (denom x) (numer y))))
  (define (equ-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero-rat? x)
    (=zero? (numer x)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ-rat?)
  (put '=zero? '(rational) =zero-rat?)
  ;; exercise 2.83
  (define (rational->real x)
    (let ((q (contents x)))
      (make-real (/ (numer q) (denom q)))))
  (put-coercion 'rational 'real rational->real)

  ;; exercise 2.85
  (define (project r)
    (let ((x (contents r)))
      (make-integer (round (/ (numer x)
                              (denom x))))))
  (put-coercion 'rational 'integer project)

  ;; exercise 2.86
  (put 'sq-rt '(rational)
       (lambda (r)
         (let ((root (sqrt (/ (numer r) (denom r)))))
           (make-complex-from-real-imag (make-real (real-part root))
                                        (make-real (imag-part root))))))
  (put 'cosine '(rational)
       (lambda (r)
         (make-real (cos (/ (numer r) (denom r))))))
  (put 'sine '(rational)
       (lambda (r)
         (make-real (sin (/ (numer r) (denom r))))))
  (put 'arctan '(rational rational)
       (lambda (r s)
         (let ((x (/ (numer r) (denom r)))
               (y (/ (numer s) (denom s))))
           (make-real (atan x y)))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))    
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real) =)
  (put '=zero? '(real)
       (lambda (x) (= 0 x)))
  (put 'make 'real
       (lambda (x) (if (real? x)
                       (tag x)
                       (error "Non-real value -- MAKE-REAL" x))))
  ;; exercise 2.83
  (define (real->complex r)
    (let ((x (contents r)))
      (make-complex-from-real-imag (tag x) (tag 0))))
  (put-coercion 'real 'complex real->complex)
  
  ;; exercise 2.85
  (define (project r)
    (let ((x (contents r)))
      (make-rational (inexact->exact (numerator x))
                     (inexact->exact (denominator x)))))
  (put-coercion 'real 'rational project)

  ;; exercise 2.86
  (put 'sq-rt '(real)
       (lambda (x)
         (let ((root (sqrt x)))
           (make-complex-from-real-imag (tag (real-part root))
                                        (tag (imag-part root))))))
  (put 'cosine '(real)
       (lambda (x)
         (tag (cos x))))
  (put 'sine '(real)
       (lambda (x)
         (tag (sin x))))
  (put 'arctan '(real real)
       (lambda (x y)
         (tag (atan x y))))
  'done)

(define (make-real x)
  ((get 'make 'real) x))

(define (install-rectangular-package)
  ;; internal procedures
  ;(define (square x) (* x x))
  (define (real-pt z) (car z))
  (define (imag-pt z) (cdr z))
  (define (make-from-real-imag x y)
    (if (and (data-lower-than? x 'complex) (data-lower-than? y 'complex))
        (cons (drop x) (drop y))
        (error "Invalid real or imaginary type -- MAKE-FROM-REAL-IMAG"
               (list x y))))
  (define (mag z)
    (sq-rt (add (square (real-pt z))
                (square (imag-pt z)))))
  (define (ang z)
    (arctan (imag-pt z) (real-pt z)))
  (define (make-from-mag-ang r a)
    (if (and (data-lower-than? r 'complex) (data-lower-than? a 'complex))
        (cons (mul r (cosine a)) (mul r (sine a)))
        (error "Invalid magnitude or angle type -- MAKE-FROM-MAG-ANG"
               (list r a))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-pt '(rectangular) real-pt)
  (put 'imag-pt '(rectangular) imag-pt)
  (put 'mag '(rectangular) mag)
  (put 'ang '(rectangular) ang)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  ;(define (square x) (* x x))
  (define (mag z) (car z))
  (define (ang z) (cdr z))
  (define (make-from-mag-ang r a)
    (if (and (data-lower-than? r 'complex) (data-lower-than? a 'complex))
        (cons (drop r) (drop a))
        (error "Invalid magnitude or angle type -- MAKE-FROM-MAG-ANG"
               (list r a))))
  (define (real-pt z)
    (mul (mag z) (cosine (ang z))))
  (define (imag-pt z)
    (mul (mag z) (sine (ang z))))
  (define (make-from-real-imag x y)
    (if (and (data-lower-than? x 'complex) (data-lower-than? y 'complex))
        (cons (sq-rt (add (square x) (square y)))
              (arctan y x))
        (error "Invalid real or imaginary type -- MAKE-FROM-REAL-IMAG"
               (list x y))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-pt '(polar) real-pt)
  (put 'imag-pt '(polar) imag-pt)
  (put 'mag '(polar) mag)
  (put 'ang '(polar) ang)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-pt z) (apply-generic 'real-pt z))
(define (imag-pt z) (apply-generic 'imag-pt z))
(define (mag z) (apply-generic 'mag z))
(define (ang z) (apply-generic 'ang z))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-pt z1) (real-pt z2))
                         (add (imag-pt z1) (imag-pt z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-pt z1) (real-pt z2))
                         (sub (imag-pt z1) (imag-pt z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (mag z1) (mag z2))
                       (add (ang z1) (ang z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (mag z1) (mag z2))
                       (sub (ang z1) (ang z2))))

  ;; exercise 2.79
  (define (equ-complex? z1 z2)
    (and (equ? (real-pt z1) (real-pt z2))
         (equ? (imag-pt z1) (imag-pt z2))))

  ;; exercise 2.80
  (define (=zero-complex? z)
    (equ? (mag z) 0))

  ;; test procedure for 2.82
  (define (add3-complex z1 z2 z3)
    (make-from-real-imag (add (real-pt z1)
                              (real-pt z2)
                              (real-pt z3))
                         (add (imag-pt z1)
                              (imag-pt z2)
                              (imag-pt z3))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ;; exercise 2.77
  (put 'real-pt '(complex) real-pt)
  (put 'imag-pt '(complex) imag-pt)
  (put 'mag '(complex) mag)
  (put 'ang '(complex) ang)

  ;; exercise 2.79
  (put 'equ? '(complex complex) equ-complex?)

  ;; exercise 2.80
  (put '=zero? '(complex) =zero-complex?)

  ;; exercise 2.85 (changed by exercise 2.86)
  (define (project c)
    (let ((z (contents c)))
      (raise-until 'real (real-pt z))))
  (put-coercion 'complex 'real project)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;; Exercise 2.83
(define (raise x)
  (define (next tower)
    (if (or (null? tower) (null? (cdr tower)))
        (error "Cannot raise top of type-tower -- RAISE" (type-tag x))
        (let ((coerce (get-coercion (type-tag x) (cadr tower))))
          (if coerce
              (coerce x)
              (error "No raise procedure -- RAISE"
                     (list (type-tag x) (cadr tower)))))))
  (let ((subtower (memq (type-tag x) type-tower)))
    (if subtower
        (next subtower)
        (error "Unknown type -- RAISE" (type-tag x)))))

;; Exercise 2.84
(define (higher? t1 t2)
  (let ((tower1 (memq t1 type-tower))
        (tower2 (memq t2 type-tower)))
    (cond ((not tower1)
           (error "Type not installed in tower -- HIGHER?"
                  (list t1 type-tower)))
          ((not tower2)
           (error "Type not installed in tower -- HIGHER?"
                  (list t2 type-tower)))
          (else
           (< (length tower1) (length tower2))))))

(define (highest-type types)
  (define (accum initial list)
    (if (null? list)
        initial
        (if (higher? initial (car list))
            initial
            (accum (car list) (cdr list)))))
  (if (null? types)
      (error "Non-empty list of types -- HIGHEST-TYPE")
      (accum (car types) (cdr types))))

(define (raise-all to-type args)
  (define (iter result rest)
    (if (null? rest)
        result
        (if (or (eq? (type-tag (car rest)) to-type)
                (higher? (type-tag (car rest)) to-type))
            (iter (cons (car rest)
                        result)
                  (cdr rest))
            (iter result
                  (cons (raise (car rest))
                        (cdr rest))))))
  (if (memq to-type type-tower)
      (reverse (iter '() args))
      (error "Type not installed in tower -- RAISE-ALL"
             to-type)))

(define (same-type? types)
  (cond ((null? types) #t)
        ((null? (cdr types)) #t)
        (else
         (and (eq? (car types) (cadr types))
              (same-type? (cdr types))))))

#| Updated in exercise 2.85
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond (proc (apply proc (map contents args)))
            ((same-type? type-tags)
             (error "No method for these types"
                    (list op type-tags)))
            (else
             (let ((to-type (highest-type type-tags)))
               (let ((raised-args (raise-all to-type args)))
                 (apply apply-generic (cons op raised-args)))))))))
|#

;; Exercise 2.85
(define (project x)
  (define (prev inv-tower)
    (if (or (null? inv-tower) (null? (cdr inv-tower)))
        (error "Cannot project bottom of type-tower -- PROJECT" (type-tag x))
        (let ((coerce (get-coercion (type-tag x) (cadr inv-tower))))
          (if coerce
              (coerce x)
              (error "No project procedure -- PROJECT"
                     (list (type-tag x) (cadr inv-tower)))))))
  (let ((inverted-subtower (memq (type-tag x) (reverse type-tower))))
    (if inverted-subtower
        (prev inverted-subtower)
        (error "Unknown type -- PROJECT" (type-tag x)))))

(define (drop x)
  (if (eq? (type-tag x) (car type-tower))
      x
      (let ((try (project x)))
        (if (equ? (raise try) x)
            (drop try)
            x))))

(define (data-in-tower? x)
  (and (pair? x)
       (memq (type-tag x) type-tower)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond (proc
             (let ((result (apply proc (map contents args))))
               (if (data-in-tower? result)
                   (drop result)
                   result)))
            ((same-type? type-tags)
             (error "No method for these types"
                    (list op type-tags)))
            (else
             (let ((to-type (highest-type type-tags)))
               (let ((raised-args (raise-all to-type args)))
                 (apply apply-generic (cons op raised-args)))))))))

;; Exercise 2.86
#|
Steps to allow complex parts to be lower types:
* change make-from-real-imag and make-from-mag-ang
to require installed types
* Update complex's project function to accommodate typed data
* generalize the following operations:
  - square
  - sqrt
  - cos
  - sin
  - atan
* change complex, rectangular, polar package to use generic operations
|#
(define (lower? t1 t2)
  (let ((tower1 (memq t1 type-tower))
        (tower2 (memq t2 type-tower)))
    (cond ((not tower1)
           (error "Type not installed in tower -- HIGHER?"
                  (list t1 type-tower)))
          ((not tower2)
           (error "Type not installed in tower -- HIGHER?"
                  (list t2 type-tower)))
          (else
           (> (length tower1) (length tower2))))))

(define (data-lower-than? datum type)
  (and (data-in-tower? datum)
       (lower? (type-tag datum) type)))

(define (raise-until type data)
  (if (data-lower-than? data type)
      (let ((raised (raise data)))
        (raise-until type raised))
      data))

(define (square x) (mul x x))
(define (sq-rt x) (apply-generic 'sq-rt x))
(define (cosine x) (apply-generic 'cosine x))
(define (sine x) (apply-generic 'sine x))
(define (arctan x y) (apply-generic 'arctan x y))