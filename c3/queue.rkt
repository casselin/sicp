#lang sicp
(#%provide (all-defined))
;; Section 3.3.2 - Representing Queues

(define (make-queue)
  (cons '() '()))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

;; Exercise 3.21
#|
The interpreter is printing the return value of the queue procedures, which
is the queue itself. Since the queue is simply a pair of pointers to lists,
the interpreter's response simply prints the pair of pointers to the front
and rear of the underlying list. This is why it looks as if the item is
inserted twice; the last item in the queue will always be displayed twice.
Furthermore, the queue is considered empty only when the front pointer
is null, which explains why, after deleting the last item, the interpreter
still prints the last-deleted item.
|#

(define (print-queue queue)
  (display (front-ptr queue)))

;; Exercise 3.22
(define (make-queue2)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
             (set-front-ptr! (cdr front-ptr)))))
    (define (dispatch m)
      (cond ((eq? m 'empty?) (empty-queue?))
            ((eq? m 'front) (front-queue))
            ((eq? m 'insert!) insert-queue!)
            ((eq? m 'delete!) (delete-queue!))
            ((eq? m 'print) (display front-ptr))
            (else
             (error "Unknown request -- MAKE-QUEUE"
                    m))))
    dispatch))