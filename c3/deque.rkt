#lang sicp
;;; Exercise 3.23
;; doubly-linked list procedures
; constructor
(define (make-node data prev next)
  (cons data (cons prev next)))

; predicates
(define (triple? x)
  (and (pair? x)
       (pair? (cdr x))
       (not (pair? (cddr x)))))

(define (jump-one dlist)
    (if (triple? dlist)
        (next-node dlist)
        '()))
(define (jump-two dlist)
  (jump-one (jump-one dlist)))
(define (cycle-dlist? x)
  (define (loop slow fast)
    (cond ((or (null? fast)
               (null? slow))
           #f)
          ((eq? slow fast) #t)
          (else
           (loop (jump-one slow) (jump-two fast)))))
  (loop x (jump-two x)))

; selectors
(define (data-node node)
  (car node))
(define (prev-node node)
  (cadr node))
(define (next-node node)
  (cddr node))

; mutators
(define (set-data-node! node data)
  (set-car! node data))
(define (set-prev-node! node prev)
  (set-car! (cdr node) prev))
(define (set-next-node! node next)
  (set-cdr! (cdr node) next))
(define (link-nodes! first-node second-node)
  (set-next-node! first-node second-node)
  (set-prev-node! second-node first-node))

;; deque procedures
; constructors
(define (make-deque)
  (cons '() '()))

; internal procedures
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

; predicates
(define (empty-deque? deque)
  (or (null? (front-ptr deque))
      (null? (rear-ptr deque))))

; selectors
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called on empty deque"
             (print-deque deque))
      (data-node (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called on an empty deque"
             (print-deque deque))
      (data-node (rear-ptr deque))))

; mutators
(define (front-insert-deque! deque item)
  (let ((new-node (make-node item '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node))
          (else
           (link-nodes! new-node (front-ptr deque))
           (set-front-ptr! deque new-node)))))

(define (rear-insert-deque! deque item)
  (let ((new-node (make-node item '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-node)
           (set-rear-ptr! deque new-node))
          (else
           (link-nodes! (rear-ptr deque) new-node)
           (set-rear-ptr! deque new-node)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called on empty deque"
                (print-deque deque)))
         (else
          (if (null? (next-node (front-ptr deque)))
              (set-front-ptr! deque (next-node (front-ptr deque)))
              (begin
                (set-front-ptr! deque (next-node (front-ptr deque)))
                (set-prev-node! (front-ptr deque) '()))))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called on empty deque"
                (print-deque deque)))
        (else
         (if (null? (prev-node (rear-ptr deque)))
             (set-rear-ptr! deque (prev-node (rear-ptr deque)))
             (begin
               (set-rear-ptr! deque (prev-node (rear-ptr deque)))
               (set-next-node! (rear-ptr deque) '()))))))

; printers
; print-deque
(define (print-deque deque)
  (define (loop node)
    (if (null? node)
        '()
        (cons (data-node node) (loop (next-node node)))))
  (if (cycle-dlist? deque)
      (error "deque contains cycle -- PRINT")
      (loop (front-ptr deque))))