#lang sicp
(#%require "registers.rkt")
;;; Section 5.3 Storage Allocation and Garbage Collection
;; Exercise 5.21
;a
(define count-leaves-rec
  (make-machine
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list 'not not)
         (list 'car car)
         (list 'cdr cdr)
         (list '+ +))
   '(controller
       (assign continue (label done))
     loop
       (test (op null?) (reg tree))
       (branch (label base-case-null))
       (assign temp (op pair?) (reg tree))
       (test (op not) (reg temp))
       (branch (label base-case-leaf))
       ;;set up to compute (count-leaves (car tree))
       (save continue)
       (assign continue (label after-car))
       (save tree)
       (assign tree (op car) (reg tree))
       (goto (label loop))
     after-car
       (restore tree)
       ;(restore continue)
       ;;set up to compute (count-leaves (cdr tree))
       (assign tree (op cdr) (reg tree))
       ;(save continue)
       (assign continue (label after-cdr))
       (save count)
       (goto (label loop))
     after-cdr
       (assign temp (reg count))
       (restore count)
       (restore continue)
       (assign count (op +) (reg count) (reg temp))
       (goto (reg continue))
     base-case-null
       (assign count (const 0))
       (goto (reg continue))
     base-case-leaf
       (assign count (const 1))
       (goto (reg continue))
     done)))
;b
(define count-leaves-iter
  (make-machine
   (list (list 'null? null?)
         (list 'pair? pair?)
         (list 'not not)
         (list 'car car)
         (list 'cdr cdr)
         (list '+ +))
   '(controller
       (assign continue (label done))
       (assign count (const 0))
     iter
       (test (op null?) (reg tree))
       (branch (label base-case-null))
       (assign temp (op pair?) (reg tree))
       (test (op not) (reg temp))
       (branch (label base-case-leaf))
       ;;set up to compute (count-iter (car tree) count)
       (save continue)
       (assign continue (label after-car))
       (save tree)
       (assign tree (op car) (reg tree))
       (goto (label iter))
     after-car
       (restore tree)
       (restore continue)
       (assign tree (op cdr) (reg tree))
       (goto (label iter))
     base-case-null
       (goto (reg continue))
     base-case-leaf
       (assign count (op +) (reg count) (const 1))
       (goto (reg continue))
     done)))

;; Exercise 5.22
(define append-machine
  (make-machine
   (list (list 'null? null?)
         (list 'cons cons)
         (list 'car car)
         (list 'cdr cdr))
   '(controller
       (assign continue (label done))
     loop
       (test (op null?) (reg x))
       (branch (label base-case))
       ;;set up to compute (append (cdr x) y)
       (save continue)
       (assign continue (label after-cdr))
       (assign item (op car) (reg x))
       (save item)
       (assign x (op cdr) (reg x))
       (goto (label loop))
     after-cdr
       (restore item)
       (restore continue)
       (assign result (op cons) (reg item) (reg result))
       (goto (reg continue))
     base-case
       (assign result (reg y))
       (goto (reg continue))
     done)))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define append!-machine
  (make-machine
   (list (list 'null? null?)
         (list 'cdr cdr)
         (list 'set-cdr! set-cdr!))
   '(controller
       (assign pair (reg x))
     loop
       (assign temp (op cdr) (reg pair))
       (test (op null?) (reg temp))
       (branch (label last-pair-found))
       ;;loop with (cdr pair)
       (assign pair (op cdr) (reg pair))
       (goto (label loop))
     last-pair-found
       (perform (op set-cdr!) (reg pair) (reg y))
     done)))