#lang sicp

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))

;; Exercise 2.56
(define (make-exponentiation base power)
  (cond ((=number? power 0) 1)
        ((=number? power 1) base)
        ((and (number? base) (number? power)) (expt base power))
        (else
         (list '** base power))))
         
(define (exponentiation? p)
  (and (pair? p) (eq? (car p) '**)))
(define (base p) (cadr p))
(define (exponent p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((and (exponentiation? exp) (number? (exponent exp)))
         (make-product (exponent exp)
                       (make-product (make-exponentiation
                                      (base exp)
                                      (- (exponent exp) 1))
                                     (deriv (base exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; Exerise 2.57
(define (make-sum a1 a2 . as)
  (define (helper a)
    (if (null? as)
        a
        (append (list '+ a) as)))
  (cond ((=number? a1 0) (helper a2))
        ((=number? a2 0) (helper a1))
        ((and (number? a1) (number? a2)) (helper (+ a1 a2)))
        (else (append (list '+ a1 a2) as))))

(define (augend s)
  (if (null? (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(define (make-product m1 m2 . ms)
  (define (helper a)
    (if (null? ms)
        a
        (append (list '* a) ms)))
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) (helper m2))
        ((=number? m2 1) (helper m1))
        ((and (number? m1) (number? m2)) (helper (* m1 m2)))
        (else (append (list '* m1 m2) ms))))

(define (multiplicand p)
  (if (null? (cdddr p))
      (caddr p)
      (cons '* (cddr p))))

;; Exercise 2.58
; a
(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum2? exp)
         (make-sum2 (deriv2 (addend2 exp) var)
                   (deriv2 (augend2 exp) var)))
        ((product2? exp)
         (make-sum2
          (make-product2 (multiplier2 exp)
                        (deriv2 (multiplicand2 exp) var))
          (make-product2 (deriv2 (multiplier2 exp) var)
                        (multiplicand2 exp))))
        ((and (exponentiation2? exp) (number? (exponent2 exp)))
         (make-product2 (exponent2 exp)
                       (make-product2 (make-exponentiation2
                                      (base2 exp)
                                      (- (exponent2 exp) 1))
                                     (deriv2 (base2 exp) var))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (sum2? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '+)))
(define (addend2 s) (car s))
(define (augend2 s) (caddr s))

(define (product2? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '*)))
(define (multiplier2 p) (car p))
(define (multiplicand2 p) (caddr p))

(define (exponentiation2? x)
  (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '**)))
(define (base2 p) (car p))
(define (exponent2 p) (caddr p))

(define (make-sum2 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product2 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentiation2 base power)
  (cond ((=number? power 0) 1)
        ((=number? power 1) base)
        ((and (number? base) (number? power)) (expt base power))
        (else (list base '** power))))
; b
(define (deriv3 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum3? exp)
         (make-sum3 (deriv3 (addend3 exp) var)
                   (deriv3 (augend3 exp) var)))
        ((product3? exp)
         (make-sum3
          (make-product3 (multiplier3 exp)
                        (deriv3 (multiplicand3 exp) var))
          (make-product3 (deriv3 (multiplier3 exp) var)
                        (multiplicand3 exp))))
        ((and (exponentiation3? exp) (number? (exponent3 exp)))
         (make-product3 (exponent3 exp)
                        (make-exponentiation3
                         (base3 exp)
                         (- (exponent3 exp) 1))
                        (deriv3 (base3 exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (foldl proc init lst)
  (if (null? lst)
      init
      (foldl proc (proc init (car lst)) (cdr lst))))

(define (singleton? x)
  (and (pair? x)
       (not (null? x))
       (null? (cdr x))))

(define (wrap x)
  (list x))

(define (wrap-item x)
  (if (pair? x)
      x
      (wrap x)))

(define (unwrap-singleton x)
  (if (singleton? x)
      (car x)
      x))

(define (sublist-before item x)
  (cond ((null? x) '())
        ((eq? item (car x)) '())
        (else
         (cons (car x) (sublist-before item (cdr x))))))

(define (sum3? x)
  (if (memq '+ x)
      true
      false))
(define (addend3 s)
  (unwrap-singleton (sublist-before '+ s)))
(define (augend3 s)
  (unwrap-singleton (cdr (memq '+ s))))
(define (make-sum3 a1 a2 . as)
 (define (sum-pair a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else
         (append (wrap-item a1) '(+) (wrap-item a2)))))
  (foldl sum-pair (sum-pair a1 a2) as))

(define (wrap-not-op x op?)
  (if (and (pair? x) (op? x))
      x
      (wrap x)))

(define (product3? x)
  (cond ((sum3? x) false)
        ((memq '* x) true)
        (else false)))
(define (multiplier3 p)
  (unwrap-singleton (sublist-before '* p)))
(define (multiplicand3 p)
  (unwrap-singleton (cdr (memq '* p))))
(define (make-product3 m1 m2 . ms)
 (define (mul-pair m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
         (append (wrap-not-op m1 product3?)
                 '(*)
                 (wrap-not-op m2 product3?)))))
  (foldl mul-pair (mul-pair m1 m2) ms))

(define (exponentiation3? x)
  (cond ((sum3? x) false)
        ((product3? x) false)
        ((memq '** x) true)
        (else false)))
(define (base3 p)
  (unwrap-singleton (sublist-before '** p)))
(define (exponent3 p)
  (unwrap-singleton (cdr (memq '** p))))
(define (make-exponentiation3 b p)
  (cond ((=number? p 0) 1)
        ((=number? p 1) b)
        ((and (number? p) (number? b)) (expt b p))
        (else
         (append (wrap-not-op b exponentiation3?)
                 '(**)
                 (wrap-not-op p exponentiation3?)))))