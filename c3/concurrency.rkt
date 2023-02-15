#lang sicp

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ;busy-waiting
            (eq? m 'release) (clear! cell)))
  the-mutex))

(define (clear! cell)
  (set-car! cell false))

; note: test-and-set! must be atomic to perform properly
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;; Exercise 3.47
; a
(define (make-semaphore n)
  (let ((acquires-remaining n) (mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (zero? acquires-remaining)
                 (begin
                   (mutex 'release)
                   (the-semaphore 'acquire)) ;busy-waiting
                 (begin
                   (set! acquires-remaining (dec acquires-remaining))
                   (mutex 'release))))
            ((eq? m 'release)
             (mutex 'acquire)
             (set! acquires-remaining (inc acquires-remaining))
             (mutex 'release))))
    the-semaphore))

; b
; test-and-transform! should be implemented atomically
; eg. with the without-interrupts procedure in MIT Scheme
(define (test-and-transform! cell test transformer)
  (if (test (car cell))
      true
      (begin (set-car! cell (transformer (car cell)))
             false)))

(define (test-and-dec! cell)
  (test-and-transform!
   cell zero? dec))

; must be made atomic
(define (inc! cell)
  (set-car! cell (inc (car cell))))

(define (make-semaphore2 n)
  (let ((cell (list n)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-dec! cell)
                 (the-semaphore 'acquire))) ;busy-waiting
            ((eq? m 'release)
             (inc! cell))))
    the-semaphore))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

; make-account-and-serializer procedure from page 309
(define (make-account id balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'id) id)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

;; Exercise 3.48
#|
The technique of working with the lowest ID account first avoids the deadlock
because both processes will be forced to interact with the same serializer
as their first action. Thus the described deadlock cannot occur because the
second process will need to wait for the first process to finish with a1.
Then the processes will interact with the serializer for a2. At no point
will the two processes need to "trade" control of the accounts, which would
be the cause of the deadlock.
|#
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< (account1 'id) (account2 'id))
        ((serializer1 (serializer2 exchange))
         account1 account2)
        ((serializer2 (serializer1 exchange))
         account1 account2))))