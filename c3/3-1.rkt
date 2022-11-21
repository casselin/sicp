#lang sicp

;;; Chapter 3.1

;; Exercise 3.1
(define (make-accumulator sum)
  (lambda (n)
    (set! sum (+ sum n))
    sum))

;; Exercise 3.2
(define (make-monitored f)
  (let ((counter 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) counter)
            ((eq? m 'reset-count) (set! counter 0))
            (else
             (set! counter (inc counter))
             (f m))))
    dispatch))

;; Exercise 3.3/3.4
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (auth-handlers dispatcher)
    (let ((count 0))
      (define (success m)
        (set! count 0)
        (dispatcher m))
      (define (failure m)
        (set! count (inc count))
        (if (= count 7)
            (lambda (_) (call-the-cops))
            (lambda (_) "Incorrect password")))
      (cons success failure)))
  (define (call-the-cops)
    "We called the cops")
  (define (make-auth-dispatch success failure)
    (lambda (pw m)
      (if (eq? pw password)
          (success m)
          (failure m))))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  (let ((dispatchers (auth-handlers dispatch)))
    (make-auth-dispatch (car dispatchers) (cdr dispatchers))))