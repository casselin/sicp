#lang sicp

;; Huffman Encoding Trees

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; Decoding procedure

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)     ; symbol
                               (cadr pair))   ; frequency
                    (make-leaf-set (cdr pairs))))))

;; Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
#|
> (decode sample-message sample-tree)
= (A D A B B C A)
|#

;; Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (and (leaf? tree) (eq? symbol (symbol-leaf tree)))
      '()
      (let ((left-symbols (symbols (left-branch tree)))
            (right-symbols (symbols (right-branch tree))))
        (cond ((memq symbol left-symbols)
               (cons 0 (encode-symbol symbol (left-branch tree))))
              ((memq symbol right-symbols)
               (cons 1 (encode-symbol symbol (right-branch tree))))
              (else (error "bad symbol -- ENCODE-SYMBOL" symbol))))))

;; Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge trees)
  (cond ((null? trees)
         (error "bad pairs -- SUCCESSIVE-MERGE"))
        ((null? (cdr trees)) (car trees))
        (else
         (let ((left (cadr trees))
               (right (car trees))
               (rest (cddr trees)))
           (successive-merge (adjoin-set (make-code-tree left right)
                                         rest))))))

;; Exercise 2.70
(define alphabet '((A 2) (BOOM 1) (GET 2) (JOB 2)
                         (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define lyrics-tree (generate-huffman-tree alphabet))

(define lyrics '(GET A JOB
                     SHA NA NA NA NA NA NA NA NA
                     GET A JOB
                     SHA NA NA NA NA NA NA NA NA
                     WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                     SHA BOOM))
#|
> (length (encode lyrics lyrics-tree))
= 84

A fixed length code would need log2(8) = 3 bits per symbol.
There are (length lyrics) = 36 symbols in the message for a
total of 36 * 3 = 108 bits to encode the message.
|#

;; Exercise 2.71
#|
Huffman tree for n=10:
{0 1 2 3 4 5 6 7 8 9} 1023
├── 9 512
└── {0 1 2 3 4 5 6 7 8} 511
    ├── 8 256
    └── {0 1 2 3 4 5 6 7} 255
        ├── 7 128
        └── {0 1 2 3 4 5 6} 127
            ├── 6 64
            └── {0 1 2 3 4 5} 63
                ├── 5 32
                └── {0 1 2 3 4} 31
                    ├── 4 16
                    └── {0 1 2 3} 15
                        ├── 3 8
                        └── {0 1 2} 7
                            ├── 2 4
                            └── {0 1} 3
                                ├── 1 2
                                └── 0 1

The most frequent symbol uses 1 bit and the least frequent
symbol requires bits equal to the height of the tree, which is
n-1 in general.
|#

;; Exercise 2.72
#|
The order of growth of the encode-symbol procedure depends on the structure
of the huffman encoding tree.

In the best case scenario, the tree is perfectly balanced. In this scenario,
the procedure will require at most log(n) steps and each step must search the
symbol list; a process with linear growth. Thus the encode-symbol procedure
grows as O(nlogn), where n is the number of symbols, for perfectly balanced
huffman trees.

The worst case scenario is that which is described in Exercise 2.71. In this
scenario, the height of the huffman tree is n-1. Now encode-symbol will
require at most n-1 steps and each step grows linearly. This results in a
growth that is quadratic in the number of symbols.
|#