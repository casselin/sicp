#lang sicp

;; 2.3.3 Representing Sets
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '{})
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

;; Exercise 2.60
#|
element-of-set? and intersection-set are the same procedures as above,
and as such have the same efficiency as before
|#
(define (adjoin-set-dup x set)
  (cons x set))

(define (union-set-dup set1 set2)
  (append set1 set2))
#|
adjoin-set-dup and union-set-dup now grow as O(1) and O(n) versus
O(n) and O(n^2) respectively.

An application that makes heavy use of adjoining and taking unions
and can tolerate the memory space overhead would benefit from this
representation of sets.
|#

;; Sets as ordered lists
(define (element-of-set-ord? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set-ord? x (cdr set)))))

(define (intersection-set-ord set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-ord (cdr set1)
                                              (cdr set2))))
              ((< x1 x2)
               (intersection-set-ord (cdr set1)
                                     set2))
              ((< x2 x1)
               (intersection-set-ord set1
                                     (cdr set2)))))))

;; Exercise 2.61
(define (adjoin-set-ord x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set))
         (cons x set))
        (else
         (cons (car set) (adjoin-set-ord x (cdr set))))))

;; Exercise 2.62
(define (union-set-ord set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set-ord (cdr set1)
                                          (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set-ord (cdr set1)
                                          set2)))
                 ((< x2 x1)
                  (cons x2 (union-set-ord set1
                                          (cdr set2)))))))))

;; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-tree? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set-tree? x (left-branch set)))
        ((> x (entry set))
         (element-of-set-tree? x (right-branch set)))))

(define (adjoin-set-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set-tree x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set-tree x (right-branch set))))))

;; Exercise 2.63
; a
#|
Both procedures produce the same result for every tree.
In particular, the list produced by both procedures for
the trees in Figure 2.16 is
(1 3 5 7 9 11)
|#
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; b
#|
tree->list-1 calls append to join the left and right branch lists, which is
an O(n) operation. Assuming the tree is balanced, append will be called
log(n) times, thus tree->list-1 grows as O(nlog(n))

tree->list-2 creates the list through a sequence of cons operations, which
grows as O(1). Cons will be called for every entry in the tree, thus
tree->list-2 grows as O(n).
|#

;; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
; a
#|
partial-tree first recursively builds a balanced left subtree with the
first (n-1)/2 elements of the list, then recursively builds a balanced
right subtree with the remaining elements (after reserving the first
element for the root of the tree). Since the left tree is built with
the first (n-1)/2 elements, any tree that is not perfectly balanced
is biased towards the right subtrees.
(1 3 5 7 9 11) as a tree is

        .——— 11
       |
    .——— 9
   |   |
   |    `——— 7
   |
— 5
   |
   |    .——— 3
   |   |    
    `——— 1
|#

; b
#|
list->tree combines each left and right subtree using make-tree, which
grows as O(1). partial-tree will call make-tree for every element in
the list, for a total of n times. Thus list-tree grows as O(n).
|#

;; Exercise 2.65
(define (union-set-tree set1 set2)
  (let ((set-ord1 (tree->list-2 set1))
        (set-ord2 (tree->list-2 set2)))
    (let ((new-set (union-set-ord set-ord1 set-ord2)))
      (list->tree new-set))))

(define (intersection-set-tree set1 set2)
  (let ((set-ord1 (tree->list-2 set1))
        (set-ord2 (tree->list-2 set2)))
    (let ((new-set (intersection-set-ord set-ord1 set-ord2)))
      (list->tree new-set))))

;; Exercise 2.66
;; Dummy key procedure
(define (key x) x)

(define (lookup given-key set)
  (cond ((null? set) false)
        ((equal? given-key (key (entry set)))
         (entry set))
        ((< given-key (key (entry set)))
         (lookup given-key (left-branch set)))
        (else
         (lookup given-key (right-branch set)))))