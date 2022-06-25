#lang sicp
;;; Section 2.3

;; Exercise 2.53
#|
> (list 'a 'b 'c)
(a b c)

> (list (list 'george))
((george))

> (cdr '((x1 x2) (y1 y2)))
((y1 y2))

> (cadr '((x1 x2) (y1 y2)))
(y1 y2)

> (pair? (car '(a short list)))
#f

> (memq 'red '((red shoes) (blue socks)))
#f

> (memq 'red '(red shoes blue socks))
(red shoes blue socks)
|#

;; Exercise 2.54
(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b)))
         (eq? a b))
        ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        (else
         false)))

;; Exercise 2.55
#|
''abracadabra expands to (quote (quote abracadabra)) which is '(quote abracadabra)
ie. the list containing 'quote and 'abracadabra
Thus (car ''abracadabra) = quote, the first symbol in the list
|#