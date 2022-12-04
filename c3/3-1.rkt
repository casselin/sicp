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

;; Exercise 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ; for exercise 3.7 to test if password is correct
          ((eq? m 'touch) #t)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  (let ((dispatchers (auth-handlers dispatch)))
    (make-auth-dispatch password (car dispatchers) (cdr dispatchers))))

(define (make-auth-dispatch password success failure)
  (lambda (pw m)
    (if (eq? pw password)
        (success m)
        (failure m))))

;; Exercise 3.4
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

;; Exercise 3.5
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (distance a b)
  (abs (- a b)))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((experiment (lambda ()
                      (P (random-in-range x1 x2)
                         (random-in-range y1 y2))))
        (total-area (* (distance x1 x2)
                       (distance y1 y2))))
    (* total-area (monte-carlo trials experiment))))

;; Exercise 3.6
; assumed to exist
(define (rand-update x)
  (+ x 1))

(define random-init 0)

(define rand
  (let ((x random-init))
    (define generate
      (lambda ()
        (set! x (rand-update x))
        x))
    (define (reset new-value)
      (set! x new-value))
    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)
            (else "Unknown request -- RAND" m)))
    dispatch))

;; Exercise 3.7
(define (make-joint account password new-password)
  (if (account password 'touch)
      (let ((dispatchers (auth-handlers (lambda (m)
                                          (account password m)))))
        (make-auth-dispatch
         new-password (car dispatchers) (cdr dispatchers)))
      "Incorrect password"))

;; Exercise 3.8
(define f
  (let ((init #f))
    (lambda (x)
      (if init
          init
          (begin
            (set! init x)
            0)))))