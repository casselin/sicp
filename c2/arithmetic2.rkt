#lang sicp
;;; Section 2.5.3
;; Copy of arithmetic.rkt for the purposes of completing
;; Extended Exercise: Rational functions
(#%provide (all-defined))

(define (square x)
  (* x x))

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
; Modified for exercise 2.78
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

;; Definitions from Section 2.5

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))

  (define (scheme-number->rational n)
    (make-rational (contents n) 1))
  (put-coercion 'scheme-number 'rational scheme-number->rational)

  ;; exercise 2.79
  (put 'equ? '(scheme-number scheme-number) =)

  ;; exercise 2.80
  (put '=zero? '(scheme-number)
       (lambda (x) (= 0 x)))

  ;; exercise 2.94
  (put 'gcd '(scheme-number scheme-number)
       (lambda (a b) (tag (gcd a b))))

  ;; exercise 2.97
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  (put 'reduce '(scheme-number scheme-number)
       (lambda (n d) (map tag (reduce-integers n d))))
  'done)

(define (install-rational-package)
  ;; Modified for Extended Exercise: Rational Functions
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  ; exercise 2.97
  (define (make-rat n d)
    (let ((reduced (reduce n d)))
      (cons (car reduced) (cadr reduced))))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  (define (rational->complex r)
    (if (or (eq? 'polynomial (type-tag (numer (contents r))))
            (eq? 'polynomial (type-tag (denom (contents r)))))
        (error "Cannot coerce rational function" r)
        (let ((x (/ (numer (contents r)) (denom (contents r)))))
          (make-complex-from-real-imag x 0))))
  (put-coercion 'rational 'complex rational->complex)

  ;; exercise 2.79
  (define (equ-rat? x y)
    (and (equ? (numer x) (numer y))
         (equ? (denom x) (denom y))))

  ;; exercise 2.80
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

  ;; exercise 2.79
  (put 'equ? '(rational rational) equ-rat?)

  ;; exercise 2.80
  (put '=zero? '(rational) =zero-rat?)
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-pt z) (car z))
  (define (imag-pt z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (mag z)
    (sqrt (+ (square (real-pt z))
             (square (imag-pt z)))))
  (define (ang z)
    (atan (imag-pt z) (real-pt z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

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
  (define (mag z) (car z))
  (define (ang z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-pt z)
    (* (mag z) (cos (ang z))))
  (define (imag-pt z)
    (* (mag z) (sin (ang z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

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
    (make-from-real-imag (+ (real-pt z1) (real-pt z2))
                         (+ (imag-pt z1) (imag-pt z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-pt z1) (real-pt z2))
                         (- (imag-pt z1) (imag-pt z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (mag z1) (mag z2))
                       (+ (ang z1) (ang z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (mag z1) (mag z2))
                       (- (ang z1) (ang z2))))

  ;; exercise 2.79
  (define (equ-complex? z1 z2)
    (and (equ? (real-pt z1) (real-pt z2))
         (equ? (imag-pt z1) (imag-pt z2))))

  ;; exercise 2.80
  (define (=zero-complex? z)
    (equ? (mag z) 0))

  ;; test procedure for 2.82
  (define (add3-complex z1 z2 z3)
    (make-from-real-imag (+ (real-pt z1)
                            (real-pt z2)
                            (real-pt z3))
                         (+ (imag-pt z1)
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

  ;; test procedure for 2.82
  (put 'add3 '(complex complex complex)
       (lambda (z1 z2 z3) (tag (add3-complex z1 z2 z3))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Load base packages for arithmetic system
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;; Exercise 2.77
#|
First, these lines place procedures in the operation-and-type table
for data of the (complex) type. The procedure in the table is the generic
procedure for magnitude, which in turn calls apply-generic. apply-generic
strips the (complex) tag off of the data, and then calls the generic
magnitude procedure again. This time, either (polar) or (rectangular) is
removed, and the proper magnitude procedure is invoked with the data.
To illustrate:

> (magnitude '(complex rectangular 3 . 4))
= (apply-generic 'magnitude '(complex rectangular 3 . 4))
* this call to apply-generic dispatches to the generic magnitude procedure
= (magnitude '(rectangular 3 .4))
= (apply-generic 'magnitude '(rectangular 3 . 4))
* this call to apply-generic dispatches to the rectangular version
= (magnitude (3 . 4))
= 5
|#

;; Exercise 2.78
#|
Inserted above
|#

;; Exercise 2.79
(define (equ? x y) (apply-generic 'equ? x y))

;; Exercise 2.80
(define (=zero? x) (apply-generic '=zero? x))

;; Exercise 2.81
; a
#|
With the proposed changes if apply-generic is called with two of the same
arguments for an operation that is not found in the operation table, then
apply-generic will enter an infinite loop.
|#

; b
#|
The proposed changes are not necessary because the only case where
apply-generic will attempt to coerce the types to their own type is
when there is no procedure for those types in the operations table.
In this case, since there will be no appropriate coercion procedures
in the coercion table, apply-generic will report an error. Thus
apply-generic works correctly. However, checking for the coercion
procedures is wasted effort, and such a situation could be reported
as a different error for more transparency.
|#

;; Exercise 2.82
(define (coerce-all to-type args)
  (define (iter result rest)
    (if (null? rest)
        result
        (let ((from-type (type-tag (car rest))))
          (if (eq? from-type to-type)
              (iter (cons (car rest)
                          result)
                    (cdr rest))
              (let ((from->to (get-coercion from-type to-type)))
                (if from->to
                    (iter (cons (from->to (car rest))
                                result)
                          (cdr rest))
                    #f))))))
  (let ((res (iter '() args)))
    (if res
        (reverse res)
        #f)))

(define (find-coercion type-tags args)
  (define (iter types)
    (if (null? types)
        #f
        (let ((coerced-args (coerce-all (car types) args)))
          (if coerced-args
              coerced-args
              (iter (cdr types))))))
  (iter type-tags))

(define (same-type? types)
  (cond ((null? types) #t)
        ((null? (cdr types)) #t)
        (else
         (and (eq? (car types) (cadr types))
              (same-type? (cdr types))))))
         
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond (proc (apply proc (map contents args)))
            ((same-type? type-tags)
             (error "No method for these types"
                    (list op type-tags)))
            (else
             (let ((coercions (find-coercion type-tags args)))
               (if coercions
                   (apply apply-generic (cons op coercions))
                   (error "No method for these types"
                          (list op type-tags)))))))))

(define (add3 z1 z2 z3) (apply-generic 'add3 z1 z2 z3))

#|
Consider the case of an operation op on the types '(complex rational).
If provided arguments of types '(complex scheme-number), apply-generic will
never try to apply op to the (coerced) arguments, instead it will look for
a version of op that requires types '(complex complex) instead. There may
be a case where no such version of op exists, and apply-generic will fail
even though a valid operation exists.
|#

;; Exercise 2.97
(define (reduce n d)
  (apply-generic 'reduce n d))