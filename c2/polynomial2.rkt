#lang sicp

(#%require "arithmetic2.rkt")
;;; Alternate version of the polynomial package, completing
;; the exercises in Section 2.5.3 starting from exercise 2.89

;; list operations
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (foldr combiner null-value seq)
  (if (null? seq)
      null-value
      (combiner (car seq)
                (foldr combiner null-value (cdr seq)))))

(define (flatmap op seq)
  (foldr append '() (map op seq)))

(define (install-polynomial-package)
  ;; imported from dense and sparse packages
  (define (make-from-dense-termlist variable term-list)
    (cons variable ((get 'make 'dense) term-list)))
  (define (make-from-sparse-termlist variable term-list)
    (cons variable ((get 'make 'sparse) term-list)))
  ;; internal procedures
  ;; representations of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? v) (symbol? v))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (order-poly p)
    (order (term-list p)))

  #|
  (define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add (term-list p1)
                      (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))
  
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  |#
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make-dense 'polynomial
       (lambda (var terms) (tag (make-from-dense-termlist var terms))))
  (put 'make-sparse 'polynomial
       (lambda (var terms) (tag (make-from-sparse-termlist var terms))))
  (put 'order '(polynomial) order-poly)

  ;; exercise 2.87
  (define (=zero-poly? p)
    (=zero? (term-list p)))
  (put '=zero? '(polynomial) =zero-poly?)

  ;; exercise 2.88
  (define (negate-poly p)
    (make-poly (variable p)
               (negate (term-list p))))

  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (add-poly p1 (negate-poly p2))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))

  ;; exercise 2.91
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (map (lambda (L)
               (make-poly (variable p1) L))
             (div (term-list p1)
                  (term-list p2)))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (map tag (div-poly p1 p2))))

  ;; exercise 2.92
  ; This must recursively place the old variable into the coefficients
  ; of the term-list
  
  (define (variable<? variable1 variable2)
    (cond ((eq? 'const variable1) #f)
          ((eq? 'const variable2) #t)
          (else
           (string<? (symbol->string variable1)
                     (symbol->string variable2)))))

  (define (poly? datum)
    (and (pair? datum)
         (eq? 'polynomial (type-tag datum))))

  (define (raise-poly-to p var)
    (if (eq? var (variable p))
        p
        (make-poly var (raise-termlist-to (term-list p)
                                          var
                                          (variable p)))))

  (define (raise-termlist-to L new-var old-var)
    (if (empty-termlist? L)
        (make-sparse-termlist '())
        (add (raise-term-to (first-term L) new-var old-var)
             (raise-termlist-to (rest-terms L) new-var old-var))))

  (define (raise-term-to term new-var old-var)
    (let ((coef (coeff term))
          (ord (order term)))
      (if (poly? coef)
          (let ((raised (raise-poly-to (contents coef) new-var)))
            (swap-termlist-vars (term-list raised) new-var old-var ord))
          (make-term
           0
           (make-sparse-polynomial old-var
                                   (list (make-term ord coef)))))))

  (define (swap-termlist-vars termlist var old-var old-ord)
    (if (empty-termlist? termlist)
        (make-sparse-termlist '())
        (add (swap-term-vars (first-term termlist) var old-var old-ord)
             (swap-termlist-vars (rest-terms termlist) var old-var old-ord))))

  (define (swap-term-vars term var old-var old-ord)
    (let ((coef (coeff term))
          (ord (order term)))
      (make-term
       ord
       (make-sparse-polynomial old-var
                               (list (make-term old-ord coef))))))
            
  (define (add-poly p1 p2)
    (let ((var1 (variable p1))
          (var2 (variable p2)))
      (cond ((same-variable? var1 var2)
             (make-poly var1
                        (add (term-list p1)
                             (term-list p2))))
            ((variable<? var1 var2)
             (make-poly var1
                        (add (term-list p1)
                             (term-list (raise-poly-to p2 var1)))))
            (else
             (make-poly var2
                        (add (term-list (raise-poly-to p1 var2))
                             (term-list p2)))))))
  (define (mul-poly p1 p2)
    (let ((var1 (variable p1))
          (var2 (variable p2)))
      (cond ((same-variable? var1 var2)
             (make-poly var1
                        (mul (term-list p1)
                             (term-list p2))))
            ((variable<? var1 var2)
             (make-poly var1
                        (mul (term-list p1)
                             (term-list (raise-poly-to p2 var1)))))
            (else
             (make-poly var2
                        (mul (term-list (raise-poly-to p1 var2))
                             (term-list p2)))))))

  ; exercise 2.94
  (define (gcd-poly a b)
    (let ((var1 (variable a))
          (var2 (variable b)))
      (if (same-variable? var1 var2)
          (make-poly var1
                     (greatest-common-divisor (term-list a)
                                              (term-list b)))
          (error "Polynomials not in same variable"
                 (list a b)))))
  (put 'gcd '(polynomial polynomial)
       (lambda (a b) (tag (gcd-poly a b))))

  ; exercise 2.97
  (define (reduce-poly n d)
    (if (same-variable? (variable n) (variable d))
        (map (lambda (L)
               (make-poly (variable n) L))
             (reduce (term-list n)
                     (term-list d)))
        (error "Polynomials not in same variable"
               (list n d))))
  (put 'reduce '(polynomial polynomial)
       (lambda (n d) (map tag (reduce-poly n d))))
  'done)

(install-polynomial-package)

(define (make-dense-polynomial var terms)
  ((get 'make-dense 'polynomial) var terms))
(define (make-sparse-polynomial var terms)
  ((get 'make-sparse 'polynomial) var terms))

;; Exercise 2.88
#|
This is installed into the table as a negation package maintain
separation of the chapter sections and exercises. Each negation
operation should be installed in the proper type package.
See polynomial package for polynomial-related procedures
|#
(define (negate x) (apply-generic 'negate x))
(define (install-negation-package)
  ;; scheme-number
  (put 'negate '(scheme-number)
       (lambda (n) (* -1 n)))
  ;; rational
  ; internal procedures (only necessary because of clunky packaging)
  (define (numer r) (car r))
  (define (denom r) (cdr r))
  (put 'negate '(rational)
       (lambda (r) (make-rational (* -1 (numer r)) (denom r))))
  ;; complex
  (put 'negate '(complex)
       (lambda (c) (make-complex-from-real-imag
                    (* -1 (real-pt c))
                    (* -1 (imag-pt c)))))
  'done)

(install-negation-package)

;; Exercise 2.89
#|
Implementations for terms and dense term-lists are created
in packages in anticipation of exercise 2.90. Procedures will
be implemented in a manner that minimizes any changes to
implementations of other procedures (predominantly,
add-terms and mul-terms).
|#
(define (install-term-package)
  ;; internal procedures
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (=zero-term? t)
    (=zero? (coeff t)))
  (define (add-term a b)
    (if (= (order a) (order b))
        (make-term (order a)
                   (add (coeff a) (coeff b)))
        (error "CANNOT ADD TERMS OF DIFFERENT ORDER -- ADD-TERM"
               (list a b))))
  (define (mul-term a b)
    (make-term (+ (order a) (order b))
               (mul (coeff a) (coeff b))))
  (define (negate-term t)
    (make-term (order t)
               (negate (coeff t))))
  ;; interface to rest of system
  (define (tag x) (attach-tag 'term x))
  (put 'order '(term) order)
  (put 'coeff '(term) coeff)
  (put 'make 'term
       (lambda (order coeff) (tag (make-term order coeff))))
  (put '=zero? '(term) =zero-term?)
  (put 'add '(term term)
       (lambda (t1 t2) (tag (add-term t1 t2))))
  (put 'mul '(term term)
       (lambda (t1 t2) (tag (mul-term t1 t2))))
  (put 'negate '(term)
       (lambda (t) (tag (negate-term t))))

  ; coercions
  (define (term->sparse t)
    (make-sparse-termlist (list t)))
  (put-coercion 'term 'sparse term->sparse)
  (define (term->dense t)
    ((get-coercion 'sparse 'dense) (make-sparse-termlist (list t))))
  (put-coercion 'term 'dense term->dense)
  'done)

(define (make-term order coeff)
  ((get 'make 'term) order coeff))
(define (order x)
  (apply-generic 'order x))
(define (coeff x)
  (apply-generic 'coeff x))
(install-term-package)

(define (install-dense-termlist-package)
  ;; internal procedures
  (define (adjoin-term term term-list)
    (let ((next-ord (length term-list))
          (term-ord (order term))
          (coef (coeff term)))
      (cond ((=zero? term) term-list)
            ((= term-ord next-ord)
             (cons coef term-list))
            ((> term-ord next-ord)
             (adjoin-term term (cons 0 term-list)))
            (error "Cannot adjoin term of lower order -- ADJOIN-TERM"
                   (list term term-list)))))
  (define (make-termlist L)
    (if (null? L)
        (the-empty-termlist)
        (let ((t (car L)) (ts (cdr L)))
          (adjoin-term t
                       (make-termlist ts)))))
  
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (let ((ord (- (length term-list) 1))
          (coef (car term-list)))
      (make-term ord coef)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list)
    (null? term-list))

  (define (=zero-terms? L)
    (if (empty-termlist? L)
        #t
        (let ((t (first-term L)) (ts (rest-terms L)))
          (and (=zero? t) (=zero-terms? ts)))))

  (define (order-terms L)
    (- (length L) 1))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (add t1 t2)
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (mul t1 t2)
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (negate-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t (first-term L)) (ts (rest-terms L)))
          (adjoin-term (negate t)
                       (negate-terms ts)))))
  
  ;; interface to rest of system
  (define (tag x) (attach-tag 'dense x))
  (put 'first-term '(dense) first-term)
  (put 'rest-terms '(dense)
       (lambda (term-list) (tag (rest-terms term-list))))
  (put 'empty-termlist? '(dense) empty-termlist?)
  (put '=zero? '(dense) =zero-terms?)
  (put 'order '(dense) order-terms)
  (put 'add '(dense dense)
       (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'mul '(dense dense)
       (lambda (L1 L2) (tag (mul-terms L1 L2))))
  (put 'negate '(dense)
       (lambda (L) (tag (negate-terms L))))
  (put 'make 'dense
       (lambda (L) (tag (make-termlist L))))

  ;; coercions
  (define (dense->sparse L)
    (define (helper order coef-list)
      (if (null? coef-list)
          (the-empty-termlist)
          (cons (make-term order (car coef-list))
                (helper (- order 1) (cdr coef-list)))))
    (let ((term-list (contents L)))
      (make-sparse-termlist
       (filter (lambda (t) (not (=zero? t)))
               (helper (- (length term-list) 1)
                       term-list)))))
  (put-coercion 'dense 'sparse dense->sparse)

  ;; exercise 2.91
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (let ((new (mul-term-by-all-terms
                                   (make-term new-o new-c) L2)))
                         (div-terms
                          (add-terms L1 (negate-terms new))
                          L2))))
                  (let ((q (add-terms (make-termlist
                                       (list (make-term new-o new-c)))
                                      (car rest-of-result)))
                        (r (cadr rest-of-result)))
                    (list q r))))))))
  (put 'div '(dense dense)
       (lambda (L1 L2) (map tag (div-terms L1 L2))))

  ; exercise 2.94
  (define (remainder-terms L1 L2)
    (cadr (div-terms L1 L2)))

  #| rewritten in exercise 2.96
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))
  |#
  (put 'gcd '(dense dense)
       (lambda (a b) (tag (gcd-terms a b))))

  ; exercise 2.96
  (define (integerizing-factor L1 L2)
    (let ((c (coeff (first-term L2)))
          (O1 (order-terms L1))
          (O2 (order-terms L2)))
      (expt c (+ 1 (- O1 O2)))))
  (define (pseudoremainder-terms L1 L2)
    (let ((factor (integerizing-factor L1 L2)))
      (let ((new-dividend (mul-term-by-all-terms
                           (make-term 0 factor)
                           L1)))
        (cadr (div-terms new-dividend L2)))))
  (define (gcd-coeffs L)
    (if (empty-termlist? L)
        0
        (let ((first (first-term L))
              (rest (rest-terms L)))
          (gcd (coeff first) (gcd-coeffs rest)))))
  (define (div-coeffs L divisor)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (adjoin-term
         (make-term
          (order (first-term L))
          (div (coeff (first-term L)) divisor))
         (div-coeffs (rest-terms L) divisor))))
  (define (div-coeffs-by-gcd L)
    (let ((divisor (gcd-coeffs L)))
      (div-coeffs L divisor)))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (div-coeffs-by-gcd a)
        (gcd-terms b (pseudoremainder-terms a b))))
  'done)

(install-dense-termlist-package)
(define (make-dense-termlist L)
  ((get 'make 'dense) L))
(define (first-term term-list)
  (apply-generic 'first-term term-list))
(define (rest-terms term-list)
  (apply-generic 'rest-terms term-list))
(define (empty-termlist? term-list)
  (apply-generic 'empty-termlist? term-list))

;; Exercise 2.90
(define (install-sparse-termlist-package)
  ;; internal procedures
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (make-termlist L) L)
  
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (=zero-terms? L)
    (if (empty-termlist? L)
        #t
        (let ((t (first-term L)) (ts (rest-terms L)))
          (and (=zero? t) (=zero-terms? ts)))))

  (define (order-terms L)
    (order (first-term L)))

  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (add t1 t2)
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (mul t1 t2)
           (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (negate-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t (first-term L)) (ts (rest-terms L)))
          (adjoin-term (negate t)
                       (negate-terms ts)))))             
  
  ;; interface to rest of system
  (define (tag x) (attach-tag 'sparse x))
  (put 'first-term '(sparse) first-term)
  (put 'rest-terms '(sparse)
       (lambda (term-list) (tag (rest-terms term-list))))
  (put 'empty-termlist? '(sparse) empty-termlist?)
  (put '=zero? '(sparse) =zero-terms?)
  (put 'order '(sparse) order-terms)
  (put 'add '(sparse sparse)
       (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'mul '(sparse sparse)
       (lambda (L1 L2) (tag (mul-terms L1 L2))))
  (put 'negate '(sparse)
       (lambda (L) (tag (negate-terms L))))
  (put 'make 'sparse
       (lambda (L) (tag (make-termlist L))))

  ;; coercions
  (define (sparse->dense L)
    (let ((term-list (contents L)))
      (make-dense-termlist term-list)))
  (put-coercion 'sparse 'dense sparse->dense)

  ;; exercise 2.91
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (let ((new (mul-term-by-all-terms
                                   (make-term new-o new-c) L2)))
                         (div-terms
                          (add-terms L1 (negate-terms new))
                          L2))))
                  (let ((q (add-terms (make-termlist
                                       (list (make-term new-o new-c)))
                                      (car rest-of-result)))
                        (r (cadr rest-of-result)))
                    (list q r))))))))
  (put 'div '(sparse sparse)
       (lambda (L1 L2) (map tag (div-terms L1 L2))))

  ; exercise 2.94
  (define (remainder-terms L1 L2)
    (cadr (div-terms L1 L2)))

  #| rewritten in exercise 2.96
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))
  |#
  (put 'gcd '(sparse sparse)
       (lambda (a b) (tag (gcd-terms a b))))

  ; exercise 2.96
  (define (integerizing-factor L1 L2)
    (let ((c (coeff (first-term L2)))
          (O1 (order-terms L1))
          (O2 (order-terms L2)))
      (expt c (+ 1 (- O1 O2)))))
  (define (pseudoremainder-terms L1 L2)
    (let ((factor (integerizing-factor L1 L2)))
      (let ((new-dividend (mul-term-by-all-terms
                           (make-term 0 factor)
                           L1)))
        (cadr (div-terms new-dividend L2)))))
  (define (gcd-coeffs L)
    (if (empty-termlist? L)
        0
        (let ((first (first-term L))
              (rest (rest-terms L)))
          (gcd (coeff first) (gcd-coeffs rest)))))
  (define (div-coeffs L divisor)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (adjoin-term
         (make-term
          (order (first-term L))
          (div (coeff (first-term L)) divisor))
         (div-coeffs (rest-terms L) divisor))))
  (define (div-coeffs-by-gcd L)
    (let ((divisor (gcd-coeffs L)))
      (div-coeffs L divisor)))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (div-coeffs-by-gcd a)
        (gcd-terms b (pseudoremainder-terms a b))))

  ; exercise 2.97
  (define (quotient-terms L1 L2)
    (car (div-terms L1 L2)))
  (define (remove-redundant-factors termlists)
    (let ((f (apply gcd (map gcd-coeffs termlists))))
      (map (lambda (L) (div-coeffs L f)) termlists)))  
  (define (reduce-terms n d)
    (let ((g (gcd-terms n d)))
      (let ((f (integerizing-factor
                (if (< (order-terms n) (order-terms d))
                    d
                    n)
                g)))
        (let ((reduced-lists (map (lambda (L)
                                    (quotient-terms
                                     (mul-term-by-all-terms
                                      (make-term 0 f)
                                      L)
                                     g))
                                  (list n d))))
          (remove-redundant-factors reduced-lists)))))
  (put 'reduce '(sparse sparse)
       (lambda (n d) (map tag (reduce-terms n d))))
  'done)

(install-sparse-termlist-package)
(define (make-sparse-termlist L)
  ((get 'make 'sparse) L))

;; exercise 2.92
(define (scheme-number->polynomial n)
  (make-sparse-polynomial 'const (list (make-term 0 n))))
(put-coercion 'scheme-number 'polynomial
              scheme-number->polynomial)

;; exercise 2.94
(define (greatest-common-divisor a b)
  (apply-generic 'gcd a b))