#lang sicp
;;; Section 2.2

;; Exercise 2.17
(define (last-pair xs)
  (if (null? (cdr xs))
      (list (car xs))
      (last-pair (cdr xs))))

;; Exercise 2.18
(define (reverse xs)
  (define (iter ret ys)
    (if (null? ys)
        ret
        (iter (cons (car ys) ret) (cdr ys))))
  (iter nil xs))

;; Exercise 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? coin-values)
  (null? coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))
#|
The order of the list of coins does not impact the result because
the algorithm considers all possible combinations of coins.
|#

;; Exercise 2.20
(define (same-parity x . y)
  (define (helper parity y)
    (cond ((null? y) nil)
          ((= parity (remainder (car y) 2))
           (cons (car y) (helper parity (cdr y))))
          (else
           (helper parity (cdr y)))))
  (cons x (helper (remainder x 2) y)))

;; Exercise 2.21
(define (square x) (* x x))
(define (square-list1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  (map square items))

;; Exercise 2.22
#|
This iterative implementation produces the answer in the reverse order
because the first item in the input list is placed in the answer list
first. This makes it the final item in the answer list. Then the second
item in the input list is the second last, and so on. This produces an
answer list in the reverse order.

The second iterative implementation does not produce the correct answer
because to construct a single list using cons, the second argument
must be a list. Instead, the implementation produces a sequence of pairs
where the `car` of the pair is the "previous" answer and the `cdr` of the
pair is the "current" answer.
|#

;; Exercise 2.23
(define (for-each f items)
  (cond ((null? items) true)
        (else
         (f (car items))
         (for-each f (cdr items)))))

;; Exercise 2.24
#|
Interpreter output
> (list 1 (list 2 (list 3 4)))
(1 (2 (3 4)))

Box-and-pointer
(1 (2 (3 4)))     (2 (3 4))        (3 4)
[o|o]         ->    [o|o]    =>    [o|o]  =>  [o|/]
 v                   v              v          v
[1]                 [2]            [3]        [4]

Tree
(1 (2 (3 4)))
|-- 1
`-- (2 (3 4))
    |-- 2
    |-- (3 4)
    |-- 3
    `-- 4
|#

;; Exercise 2.25
#|
> (car (cdaddr (list 1 3 (list 5 7) 9)))
7
> (caar (list (list 7)))
7
> (cadadr (cadadr (cadadr
                   (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))))
7
|#

;; Exercise 2.26
#|
> (append x y)
(1 2 3 4 5 6)
> (cons x y)
((1 2 3) 4 5 6)
> (list x y)
((1 2 3) (4 5 6))
|#

;; Exercise 2.27
(define (deep-reverse items)
  (reverse
   (map (lambda (x) (if (pair? x) (reverse x) x))
        items)))

;; Exercise 2.28
(define (fringe x)
  (define (help t acc)
    (cond ((null? t) acc)
          ((not (pair? t)) (cons t acc))
          (else (help (car t) (help (cdr t) acc)))))
  (help x nil))

;; Exercise 2.29
; a
(define (make-mobile left right)
  (list left right))
(define (left-branch m)
  (car m))
(define (right-branch m)
  (cadr m))

(define (make-branch length structure)
  (list length structure))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (cadr b))

; b
(define (mobile? s)
  (pair? s))
(define (total-weight m)
  (cond ((null? m) 0)
        ((not (mobile? m)) m)
        (else
         (let ((l (left-branch m))
               (r (right-branch m)))
           (+ (total-weight (branch-structure l))
              (total-weight (branch-structure r)))))))

; c
(define (branch-torque b)
  (* (branch-length b)
     (total-weight (branch-structure b))))
(define (balanced? m)
  (define (helper m)
    (if (not (mobile? m))
        (cons m true)
        (let ((l (helper (branch-structure (left-branch m))))
              (r (helper (branch-structure (right-branch m)))))
          (cons (+ (car l)
                   (car r))
                (and (cdr l)
                     (cdr r)
                     (= (* (branch-length (left-branch m))
                           (car l))
                        (* (branch-length (right-branch m))
                           (car r))))))))
  (cdr (helper m)))

; d
#|
Changing to
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))
Only requires changing the selectors right-branch and branch-structure into
(define (right-branch m)
  (cdr m))
(define (branch-structure b)
  (cdr b))
|#

;; Exercise 2.30
(define (square-tree1 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree1 (car tree))
                    (square-tree1 (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (square sub-tree)))
       tree))

;; Exercise 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))
(define (square-tree tree)
  (tree-map square tree))

;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (l) (cons (car s) l))
                          rest)))))
#|
The recursive plan for subsets is as follows
* The subsets of the empty set is (())
* The subsets of a list x is the subsets of (cdr x) appended
with (car x) cons'd to each subset of (cdr x)
|#