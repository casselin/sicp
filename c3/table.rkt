#lang sicp
;; Section 3.3.3 - Representing Tables
;; Exercise 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

;; Exercise 3.25
(define (make-table2)
  (list '*table*))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (lookup keys table)
  (if (null? keys)
      false
      (let ((record (assoc (car keys) (cdr table))))
        (cond ((not record) false)
              ((null? (cdr keys)) (cdr record))
              (else
               (lookup (cdr keys) record))))))

(define (insert! keys value table)
  (cond ((null? keys)
         (error "Empty list of keys" keys))
        ((null? (cdr keys))
         (let ((record (assoc (car keys) (cdr table))))
           (if record
               (set-cdr! record value)
               (set-cdr! table
                         (cons (cons (car keys) value)
                               (cdr table)))))
         'ok)
        (else
         (let ((subtable (assoc (car keys) (cdr table))))
           (if subtable
               (insert! (cdr keys) value subtable)
               (begin
                 (set-cdr! table (cons (list (car keys)) (cdr table)))
                 (insert! keys value table)))))))

;; Exercise 3.26
;; binary tree procedures
; constructor
(define (make-tree entry left right)
  (list entry left right))
; selectors
(define (entry tree)
  (car tree))
(define (left tree)
  (cadr tree))
(define (right tree)
  (caddr tree))

(define (make-table3 eq-pred? lt-pred?)
  (let ((local-table (cons '*table* '())))
    (define (assoc key records)
      (cond ((null? records) false)
            ((eq-pred? key (car (entry records))) (entry records))
            ((lt-pred? key (car (entry records))) (assoc key (left records)))
            (else (assoc key (right records)))))
    (define (add-record record tree)
      (cond ((null? tree) (make-tree record '() '()))
            ((eq-pred? (car record) (car (entry tree)))
             (make-tree record (left tree) (right tree)))
            ((lt-pred? (car record) (car (entry tree)))
             (make-tree
              (entry tree) (add-record record (left tree)) (right tree)))
            (else
             (make-tree
              (entry tree) (left tree) (add-record record (right tree))))))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (set-cdr! local-table (add-record (cons key value) (cdr local-table))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))