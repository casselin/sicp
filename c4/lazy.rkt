#lang sicp
(define apply-in-underlying-scheme apply)

; Sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; Assignments and Definitions
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; Representing Expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
;; Exercise 4.33
(define (eval-quote exp env)
  (let ((q (text-of-quotation exp)))
    (if (or (null? q)
            (not (pair? q)))
        q
        (eval (convert-quoted-list q) env))))
(define (convert-quoted-list q)
  (if (null? q)
      (quote '())
      (list 'cons
            (list 'quote (car q))
            (convert-quoted-list (cdr q)))))
;

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)     ; formal parameters
                   (cddr exp))))   ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; Derived expressions
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

; deprecated by exercise 4.5 below
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                            ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (let*? exp) (tagged-list? exp 'let*))
(define (first-binding bindings) (car bindings))
(define (rest-bindings bindings) (cdr bindings))
(define (last-binding? bindings)
  (null? (cdr bindings)))
(define (make-let bindings body)
  (cons 'let (cons bindings body)))
(define (let*->nested-lets exp)
  (define (iter bindings)
    (cond ((null? bindings)
           (error "No bindings in let*" exp))
          ((last-binding? bindings)
           (make-let (list (first-binding bindings))
                     (let-body exp)))
          (else
           (make-let (list (first-binding bindings))
                     (list (iter (rest-bindings bindings)))))))
  (iter (let-bindings exp)))

(define (make-definition var value)
  (cons 'define (cons var value)))
(define (let? exp) (tagged-list? exp 'let))
(define (named-let? exp) (symbol? (cadr exp)))
(define (named-let-var exp) (cadr exp))
(define (let-bindings exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))
(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))
(define (let-parameters exp)
  (map car (let-bindings exp)))
(define (let-arguments exp)
  (map cadr (let-bindings exp)))
(define (named-let->combination exp)
  (let ((named-proc
         (make-definition (cons (named-let-var exp)
                                (let-parameters exp))
                          (let-body exp))))
    (cons (make-lambda (let-parameters exp)
                       (list named-proc
                             (cons (named-let-var exp)
                                   (let-parameters exp))))
          (let-arguments exp))))
(define (let->combination exp)
  (if (named-let? exp)
      (named-let->combination exp)
      (cons (make-lambda (let-parameters exp)
                         (let-body exp))
            (let-arguments exp))))

;; Evaluator Data Structures
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

; Representing procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; Operations on Environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (let ((var-len (length variables))
        (val-len (length values)))
    (if (= var-len val-len)
        (cons '*frame* (map cons variables values))
        (if (< var-len val-len)
            (error "Too many arguments supplied" variables values)
            (error "Too few arguments supplied" variables values)))))
(define (frame-bindings frame)
  (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))
(define (extend-environment vars vals base-env)
  (cons (make-frame vars vals) base-env))

(define (scan frame var found-op null-op)
  (let iter ((bindings (frame-bindings frame)))
    (if (null? bindings)
        (null-op)
        (if (eq? var (caar bindings))
            (found-op bindings)
            (iter (cdr bindings))))))

(define (env-loop env var found-op null-op)
  (if (eq? env the-empty-environment)
      (null-op)
      (scan (first-frame env) var
            found-op
            (lambda () (env-loop (enclosing-environment env)
                                 var found-op null-op)))))

(define (lookup-variable-value var env)
  (env-loop env var
            (lambda (f) (let ((value (cdar f)))
                           (if (eq? value '*unassigned*)
                               (error "Unassigned variable:" var)
                               value)))
            (lambda () (error "Unbound variable" var))))

(define (define-variable! var val env)
  (scan (first-frame env) var
        (lambda (f) (set-car! f (cons var val)))
        (lambda () (add-binding-to-frame! var val (first-frame env)))))

(define (set-variable-value! var val env)
  (env-loop env var
            (lambda (f) (set-car! f (cons var val)))
            (lambda () (error "Unbound variable -- SET" var))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'underlying-car car)
        (list 'underlying-cdr cdr)
        (list 'underlying-cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '< <)
        (list '<= <=)
        (list '> >)
        (list '>= >=)
        (list 'list list)
        (list 'not not)
        (list 'equal? equal?)
        (list 'even? even?)
        (list 'odd? odd?)
        (list 'newline newline)
        (list 'display display)
        ;<more primitives>
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;;; 4.2 Lazy Evaluation

;; Exercise 4.25
#|
Attempting to evaluate (factorial 5) will cause the evaluator to try to
evaluate the argument (* n (factorial (- n 1))) which will cause an
infinite loop. This definition would work in a normal-order language,
however, because the recursive loop generated by the procedure will
terminate once n is 1. This is because the recursive expression will
not be evaluated when passed as an argument to unless when n is 1.
|#

;; Exercise 4.26
#|
unless as a special form
|#
(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-cond exp) (cadr exp))
(define (unless-usual exp) (caddr exp))
(define (unless-exceptional exp) (cadddr exp))
(define (unless->if exp)
  (make-if (unless-cond exp)
           (unless-exceptional exp)
           (unless-usual exp)))
#|
There may exist a use-case for unless as a procedure passed to the map
function as in (map unless condition-list usual-list exceptional-list).
This couldn't be done if unless was a special form/derived expression.
|#

(define (lapply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))    ;changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)   ;changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

; redefined here because of the modification to the application? clause
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ;; exercise 4.33
        ((quoted? exp) (eval-quote exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ;; modified for lazy evaluation
        ((application? exp)
         (lapply (actual-value (operator exp) env)
                 (operands exp)
                 env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

; Representing thunks
(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ;replace exp with its value
           (set-cdr! (cdr obj) '())     ;forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;; Exercise 4.27
#|
(define w (id (id 10)))

;;; L-Eval input:
count
;;; L-Eval value:
1

;;; L-Eval input:
w
;;; L-Eval value:
10

;;; L-Eval input:
count
;;; L-Eval value:
2

During evaluation of the definition of w, the inner (id 10) expression
is converted to a thunk, and evaluation of the outer id procedure begins.
The set! expression is evaluated, increasing count to 1, and then the
outer id procedure returns the thunk representing (id 10). Once w is input,
the driver-loop forces evaluation of this thunk, causing the value of w
to be evaluated to 10, which also causes count to be set to 2.
|#

;; Exercise 4.28
#|
Consider the standard definition of map. The procedure passed to map will
be passed in as a thunk. Thus when map attempts to call the procedure
with the car of the list, procedure will still be wrapped in a thunk,
which eval does not have a clause to handle. This means eval will think
that this is a procedure application with the procedure named thunk, which
will cause an error.
|#

;; Exercise 4.29
#|
(define (square x) (* x x))
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else
         (+ (fib (- n 1)) (fib (- n 2))))))
(square (fib 100) (fib 100))
is a program that should run much slower without memoization.

Non-memoized:
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100

;;; L-Eval input:
count
;;; L-Eval value:
2

Memoized:
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100

;;; L-Eval input:
count
;;; L-Eval value:
1
|#

;; Exercise 4.30
;a
#|
Ben is correct about the behaviour of for-each because the procedure passed
to for-each contains only primitive procedures, which force the evaluation
of their arguments. Thus when eval-sequence is called with the body of the
lambda, it can successfully evaluate (newline) and (display x), which will
cause their side effects to occur.
|#
;b
#|
With the original eval-sequence (p1 1) is the list (1 2), but (p2 2)
returns 1. This occurs because the expression e inside the procedure
p is bound to a thunk. Thus evaluating the expression e simply retrieves
the thunk, which has no effect and is discarded when moving to the
next expression in the sequence. If eval-sequence instead uses
actual-value, then the thunk would be forced after being retrieved
by lookup-variable-value, and the effect will occur.
|#
;c
#|
Cy is correct because actual-value calls force-it on the evaluated
expression. If the evaluated expression is not a thunk, it simply
returns the object without performing any tasks.
|#
;d
#|
This seems like a situation that indicates problems in the implementation
of a program, given that the use of side effects with lazy evaluation is
dangerous or hard to comprehend. Under lazy evaluation, procedures should
only be relied upon for their return values, not for effects they may produce
(they shouldn't be producing side effects). With this in mind, Cy's approach
does seem reasonable since it does not negatively impact Ben's somewhat more
sensible use of side effects in a lazy procedure.
|#

;;; Streams as Lazy Lists
(define the-global-environment (setup-environment))

;; Installing lazy list procedures into the evaluator

;; Exercise 4.34
#|
print-lazy-pair has a limit argument to set how many elements
of a list to print (currently set to 20).
|#
(eval-definition '(define (cons x y)
                    (underlying-cons 'lazy-pair (lambda (m) (m x y))))
                 the-global-environment)
(eval-definition '(define (car z)
                    ((underlying-cdr z) (lambda (p q) p)))
                 the-global-environment)
(eval-definition '(define (cdr z)
                    ((underlying-cdr z) (lambda (p q) q)))
                 the-global-environment)

(define (lazy-pair? exp) (tagged-list? exp 'lazy-pair))

(define (print-lazy-pair obj limit)
  (define (lazy-car z)
    (force-it (lookup-variable-value 'x (procedure-environment (cdr z)))))
  (define (lazy-cdr z)
    (force-it (lookup-variable-value 'y (procedure-environment (cdr z)))))
  (if (lazy-pair? obj)
      (if (= limit 0)
          ". ."
          (cons (lazy-car obj)
                (print-lazy-pair (lazy-cdr obj) (- limit 1))))
      obj))
(define list-print-limit 20)
(define (user-print object)
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        ((lazy-pair? object)
         (display (print-lazy-pair object list-print-limit)))
        (else (display object))))

#| deprecated by exercise 4.34
(eval-definition '(define (cons x y)
                    (lambda (m) (m x y)))
                 the-global-environment)
(eval-definition '(define (car z)
                    (z (lambda (p q) p)))
                 the-global-environment)
(eval-definition '(define (cdr z)
                    (z (lambda (p q) q)))
                 the-global-environment)
|#
(eval-definition '(define (list-ref items n)
                    (if (= n 0)
                        (car items)
                        (list-ref (cdr items) (- n 1))))
                 the-global-environment)
(eval-definition '(define (map proc items)
                    (if (null? items)
                        '()
                        (cons (proc (car items))
                              (map proc (cdr items)))))
                 the-global-environment)
(eval-definition '(define (scale-list items factor)
                    (map (lambda (x) (* x factor))
                         items))
                 the-global-environment)
(eval-definition '(define (add-lists list1 list2)
                    (cond ((null? list1) list2)
                          ((null? list2) list1)
                          (else (cons (+ (car list1) (car list2))
                                      (add-lists (cdr list1) (cdr list2))))))
                 the-global-environment)

(eval-definition '(define ones (cons 1 ones))
                 the-global-environment)
(eval-definition '(define integers (cons 1 (add-lists ones integers)))
                 the-global-environment)

;; Exercise 4.32
#|
When the car of the list was strict, we were limited to infinite objects of
one dimension (ie. sequences). Now that the car is lazy, we can create objects
of more than one dimension, in particular an infinite matrix.
|#
(eval-definition '(define ones-matrix (cons ones ones-matrix))
                 the-global-environment)