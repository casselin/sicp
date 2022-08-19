#lang sicp

;;; Section 2.4

;; From Symbolic Differentiation section
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; Placeholder definitions so interpreter does not complain
(define (put op type item)
  #t)
(define (get op type)
  #t)


;; Section definitions
#|
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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))
|#

;; Exercise 2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

; a
#|
The deriv procedure now handles expressions that are a single number or
a single variable. If not, deriv dispatches on the operator symbol (which
is in prefix form) to obtain the appropriate procedure, then calls this
procedure on the subexpressions (the suffix of the argument expression).

number? and variable? cannot be assimilated into the dispatch because
the dispatch requires the use of the operator procedure to extract the
type-tag of the data. Currently, single numbers or variables are not tagged.
Thus calling the operator procedure on a number or variable will cause an
error.
|#

; b
(define (install-sum-package)
  ;; internal procedures
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (deriv-sum terms var)
    (make-sum (deriv (addend terms) var)
              (deriv (augend terms) var)))
  ;; interface to the rest of the system
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-product-package)
  ;; internal procedures
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (deriv-product factors var)
    (make-sum
     (make-product (multiplier factors)
                   (deriv (multiplicand factors) var))
     (make-product (deriv (multiplier factors) var)
                   (multiplicand factors))))
  ;; interface to the rest of the system
  (put 'deriv '* deriv-product)
  'done)

; c
(define (install-exponentiation-package)
  ;; internal procedures
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  (define (make-exponentiation base power)
    (cond ((=number? base 0) 0)
          ((=number? base 1) 1)
          ((=number? power 0) 1)
          ((=number? power 1) base)
          ((and (number? base) (number? power)) (expt base power))
          (else (list '** base power))))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (deriv-exponent exp var)
    (make-product
     (make-product
      (exponent exp)
      (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
     (deriv (base exp) var)))
  ;; interface to the rest of the system
  (put 'deriv '** deriv-exponent)
  'done)

; d
(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp) var))))
#|
This order of indexing has the arithmetic operators as the operations,
and the derivative as the type. This is effectively the transposition
of the original operator-type table.

The easiest change to make the derivative system work is to change the
indexing of the put procedures
ie. (put '+ 'deriv deriv-sum)
Stylistically, however, we should be installing the entire derivative
package, with derivative definitions for each operator we have (+, *, **),
rather than installing a package for each arithmetic operator.
|#

;; Exercise 2.74
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
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))
; a
(define (get-record employee personnel-file)
  (let ((division (type-tag personnel-file))
        (record (apply-generic 'get-record employee personnel-file)))
    (if record
        (attach-tag division record)
        #f)))
#|
get-record must retrieve an employee record from a division's files, whose
structure will vary by division. In addition, the employee record's structure
also varies by division. This would imply that division tags must be attached
to both the personnel file and each employee record inside the personnel file.
However, we can make get-record attach the division type to the employee
record. This saves each division from having to attach the division type to
each record in their file.
Each division must tag their personnel-file with their division identifier.
Each division's get-record procedure will be supplied with '(name division)
type information. As stated above, the headquarter's version of get-record
attaches the division identifier to the record, the division itself need
not burden itself with this in their implementation.
Inside the division's package:
(define (get-record employee file) ...)
(put 'get-record '(name division) get-record)
|#

; b
(define (get-salary record) (apply-generic 'get-salary record))
#|
The record must be tagged with the division that the record came from so
that apply-generic can retrieve that division's get-salary procedure.
Inside the division's package:
(define (get-salary record) ...)
(put 'get-salary '(division) get-salary)
|#

; c
#|
Note that I have changed get-record from part a to return #f instead of an
error, since handling an error has not been addressed in the book at this
time.
|#
(define (find-employee-record employee files)
  (if (null? files)
      #f
      (let ((record (get-record employee (car files))))
        (if record
            record
            (find-employee-record employee (cdr files))))))

; d
#|
First, the company must be provided a unique division identifier.
Next, the company must install their implementations for get-record,
get-salary, etc. in the company operation-and-type table.
Finally, the company must attach their division identifier to their
personnel file and make it available to headquarters.
|#

;; Exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; Exercise 2.76
#|
New operations:
  Explicit dispatch:
The new operation must be implemented such that it dispatches on all current
type.
  Data-directed:
Each installed type package must implement a new procedure for the operation,
and put the operation in the operation-and-type table.
  Message-passing:
Each data type's dispatch function must be updated to handle the new operation.

  Most appropriate:
Adding a new operation adds a new row to the operations-and-type table. Since
explicit dispatch decomposes the table into rows, explicit dispatch is the
easiest strategy to introduce new operations.

New types:
  Explicit dispatch:
Each current operation must add an implementation for the new type.
  Data-directed:
A new package must be written and installed to provide all operations
on the new type.
  Message-passing:
The new type must be written to dispatch on all the current operations.

  Most appropriate:
Adding a new type adds a new column to the operations-and-type table.
Message-passing decomposes the table into columns, so this strategy is
easiest to introduce new types.

Data-directed can be used in a way to make either situation easy.To make
new operations easy to install, packages should be created in terms of
the operations. To make new types easy to install, packages should be created
in terms of the types.
|#