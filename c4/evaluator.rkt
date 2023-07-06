#lang sicp
(#%provide (all-defined))
;;; Section 4.1 The Metacircular Evaluator
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ;; exercise 4.5
        ;((cond? exp) (eval (cond->if exp) env))
        ((cond? exp) (eval-cond exp env))
        ;; exercise 4.4
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ;; exercise 4.6
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ;; exercise 4.9
        ((do? exp) (eval (do->named-let exp) env))
        ;; exercise 4.13
        ((make-unbound!? exp) (eval-unbind exp env))
        ;; exercise 4.20
        ((letrec? exp) (eval (letrec->let exp) env))
        ((application? exp)
         (my-apply (eval (operator exp) env)
                   (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define apply-in-underlying-scheme apply)

(define (my-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

; Procedure arguments
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; Conditionals
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

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

;; Exercise 4.1
(define (list-of-values-from-left exps env)
  (if (no-operands? exps)
      '()
      (let ((leftmost-operand (eval (first-operand exps) env)))
        (cons leftmost-operand
              (list-of-values-from-left (rest-operands exps) env)))))

(define (list-of-values-from-right exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values-from-right (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest))))

#|
Note that these procedures will work regardless of the order
of evaluation of the underlying Lisp. They will not work, however,
if the underlying Lisp uses normal-order rather than applicative-
order when evaluating arguments.
|#

;; Representing Expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

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

;; Exercise 4.2
; a
#|
The problem with Louis' plan is that procedure applications are simply
defined as any expression that is a pair. This is because any expression
that is a pair that makes it to this point in eval cannot be any other
expression, so it must be an application. If the application clause is moved
before the clause for assignments, then all expressions that aren't
self-evaluating, variables, or quotes will then be treated as applications.
As a concrete example, the evaluator will take (define x 3) and attempt to
apply the procedure define with the arguments x and 3. This will fail since
define is a special form, not a procedure.
|#

; b
#|
This change is implemented by changing the procedures regarding applications:
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
|#

;; Exercise 4.3
#|
(define (eval-dd exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
         (let ((proc (get 'eval (exp-type exp))))
           (if proc
               (proc (exp-data exp)
                     env)
               (apply (eval-dd (operator exp) env)
                (list-of-values (operands exp) env)))))))
|#

#|
Each clause after variable? in the cond of the original eval will need
to have it's eval procedure installed in syntax-tree.
|#

;; Exercise 4.4
(define (and? exp) (tagged-list? exp 'and))
(define (and-predicates exp) (cdr exp))

(define (or? exp) (tagged-list? exp 'or))
(define (or-predicates exp) (cdr exp))

; and/or implemented as special forms
(define (eval-and exp env)
  (eval-and-preds (and-predicates exp) env))
(define (eval-and-preds predicates env)
  (if (null? predicates)
      'true
      (let ((eval-first (eval (car predicates) env))
            (rest (cdr predicates)))
        (cond ((null? rest) eval-first)
              ((true? eval-first)
               (eval-and-preds rest env))
              (else
               'false)))))

(define (eval-or exp env)
  (eval-or-preds (or-predicates exp) env))
(define (eval-or-preds predicates env)
  (if (null? predicates)
      'false
      (let ((eval-first (eval (car predicates) env))
            (rest (cdr predicates)))
        (if (true? eval-first)
            eval-first
            (eval-or-preds rest env)))))

; and/or implemented as derived expressions
(define (and->if exp)
  (expand-and-predicates (and-predicates exp)))
(define (expand-and-predicates predicates)
  (if (null? predicates)
      'true
      (let ((first (car predicates))
            (rest (cdr predicates)))
        (if (null? rest)
            (make-if first
                     first
                     'false)
            (make-if first
                     (expand-and-predicates rest)
                     'false)))))

(define (or->if exp)
  (expand-or-predicates (or-predicates exp)))
(define (expand-or-predicates predicates)
  (if (null? predicates)
      'false
      (let ((first (car predicates))
            (rest (cdr predicates)))
        (make-if first
                 first
                 (expand-or-predicates rest)))))

#|
The special form implementation is preferred, as it cleanly avoids the
scenario where predicates may be evaluated twice. This can be done in
the derived expression using a let expression, but would introduce
a hidden variable binding in the environment created to evaluate
the let expression. This could introduce a potential name-conflict
for a user of this evaluator.
|#

;; Exercise 4.5
#|
Similar to exercise 4.4, implementing this alternative syntax as a
derived expression can cause a predicate to be evaluated twice or
name-conflicts depending on the implementation. As such, rewriting
cond as a special form seems to be the best way to avoid both issues.
|#
(define (cond-recipient-clause? clause)
  (eq? (cadr clause) '=>))
(define (cond-recipient clause)
  (caddr clause))
(define (eval-cond exp env)
  (eval-cond-clauses (cond-clauses exp) env))
(define (eval-cond-clauses clauses env)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (cond ((cond-recipient-clause? first)
                   (error "ELSE clause cannot be an => clause -- EVAL-COND"
                          first))
                  ((null? rest)
                   (eval (sequence->exp (cond-actions first))
                         env))
                  (else
                   (error "ELSE clause isn't last -- COND->IF"
                          clauses)))
            (if (cond-recipient-clause? first)
                (let ((test (eval (cond-predicate first) env)))
                  (if (true? test)
                      ; call apply directly because test is already evaluated
                      (my-apply (eval (cond-recipient first) env) (list test))
                      (eval-cond-clauses rest env)))
                (if (true? (eval (cond-predicate first) env))
                    (eval (sequence->exp (cond-actions first)) env)
                    (eval-cond-clauses rest env)))))))

;; Exercise 4.6
#| deprecated by exercise 4.8 (to implement named lets)
(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-parameters exp)
  (map car (let-bindings exp)))
(define (let-arguments exp)
  (map cadr (let-bindings exp)))
(define (let-body exp) (cddr exp))
(define (let->combination exp)
  (cons (make-lambda (let-parameters exp)
                     (let-body exp))
        (let-arguments exp)))
|#

;; Exercise 4.7
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
#|
Adding a clause that performs (eval (let*->nested-lets exp) env) is
sufficient because each transformation is run through eval. Thus the
series of transformations proceeds as follows:
let* -> nested-lets -> combinations -> procedure application
inside the procedure application process each operator and operand will
be evaluated in turn.
|#

;; Exercise 4.8
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

;; Exercise 4.9
#| Implementing the do iteration construct as described here:
https://conservatory.scheme.org/schemers/Documents/Standards/R5RS
(do ((<variable1> <init1> <step1>) 
     ...)
   (<test> <expression> ...)
 <command> ...)

Examples:
(define (fib n)
  (do ((a 1 (+ a b))
       (b 0 a)
       (count n (- count 1)))
    ((= count 0) b)))
|#

(define (take n list)
  (cond ((= n 0) '())
        ((null? list)
         (error "List too short -- TAKE"))
        (else
         (cons (car list)
               (take (- n 1) (cdr list))))))
(define (make-named-let var bindings body)
  (list 'let var bindings body))

(define (do? exp) (tagged-list? exp 'do))
(define (do-bindings exp) (cadr exp))
(define (first-do-bind bindings) (car bindings))
(define (rest-do-binds bindings) (cdr bindings))
(define (do-var binding) (car binding))
(define (do-init binding) (cadr binding))
(define (do-step binding)
  (if (null? (cddr binding))
      (do-var binding)
      (caddr binding)))
(define (do-exit-pair exp) (caddr exp))
(define (do-exit-test exp)
  (car (do-exit-pair exp)))
(define (do-exit-exps exp)
  (cdr (do-exit-pair exp)))
(define (do-commands exp) (cdddr exp))
(define (do->named-let exp)
  (let ((binds (map (lambda (x) (take 2 x))
                    (do-bindings exp)))
        (updates (map do-step (do-bindings exp))))
    (make-named-let 'loop
                    binds
                    (make-if (do-exit-test exp)
                             (sequence->exp (do-exit-exps exp))
                             (sequence->exp (append (do-commands exp)
                                                    (list (cons 'loop
                                                                updates))))))))

;; Evaluator Data Structures
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

; Representing procedures
(define (make-procedure parameters body env)
  ; modified by 4.16c
  (list 'procedure parameters (scan-out-defines body) env))
#|
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
|#
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; Operations on Environments
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;; Exercises 4.11 and 4.12
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

#|
scan takes a frame and a variable to search for inside the frame, followed
by a procedure of one argument to be called with the remainder of the frame
where the desired variable's binding is the car of the frame, and a procedure
of no arguments to be called if the variable does not exist in the frame.
|#
(define (scan frame var found-op null-op)
  (let iter ((bindings (frame-bindings frame)))
    (if (null? bindings)
        (null-op)
        (if (eq? var (caar bindings))
            (found-op bindings)
            (iter (cdr bindings))))))
#|
env-loop takes an environment and a variable as an argument, followed
by a procedure of one argument to be called with the remainder of the frame
where the desired variable's binding is the car of the frame, and a procedure
of no arguments to be called if the variable is not found in the environment.
|#
(define (env-loop env var found-op null-op)
  (if (eq? env the-empty-environment)
      (null-op)
      (scan (first-frame env) var
            found-op
            (lambda () (env-loop (enclosing-environment env)
                                 var found-op null-op)))))

(define (lookup-variable-value var env)
  (env-loop env var
            ;; modified by exercise 4.16a
            (lambda (f) (let ((value (cdar f)))
                           (if (eq? value '*unassigned*)
                               (error "Unassigned variable:" var)
                               value)))
            ;
            (lambda () (error "Unbound variable" var))))

(define (define-variable! var val env)
  (scan (first-frame env) var
        (lambda (f) (set-car! f (cons var val)))
        (lambda () (add-binding-to-frame! var val (first-frame env)))))

(define (set-variable-value! var val env)
  (env-loop env var
            (lambda (f) (set-car! f (cons var val)))
            (lambda () (error "Unbound variable -- SET" var))))
;

;; Exercise 4.13
#|
make-unbound! will mutate the first frame of the environment where
make-unbound! is evaluated to remove the binding associated with the given
symbol. This is to avoid a situation where make-unbound! could remove a
binding from an enclosing environment that will be used by a procedure in
that environment. This could cause unexpected errors.
|#
(define (remove-binding-from-frame! var frame)
  (let find ((prev frame)
             (current (cdr frame)))
    (cond ((null? current) current)
          ((eq? var (caar current))
           (set-cdr! prev (cdr current)))
          (else
           (find current (cdr current))))))
(define (make-unbound!? exp)
  (tagged-list? exp 'make-unbound!))
(define (unbind-variable exp)
  (cadr exp))
(define (eval-unbind exp env)
  (remove-binding-from-frame! (unbind-variable exp) (first-frame env)))
#|
With current implementation of eval, need to add
((make-unbound!? exp) (eval-unbind exp env))
to cond clauses in the eval procedure
|#
;

;; Running the Evaluator as a Program
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
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

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

;; Exercise 4.14
#|
When the system version of map is used, things go wrong because the
metacircular evaluator evaluates the arguments to map, then calls the
underlying scheme's apply procedure with the underlying scheme's map
procedure and the metacircular evaluator's evaluations of the arguments
to map. Thus the map procedure expects the representations of the arguments
to be the underlying scheme's representations. Instead, it receives the
metacircular evaluator's representations instead. This mismatch will cause
error. When map is implemented as a compound procedure instead, map will
expect and be called with the same representation for procedures and will
work properly.
|#

;; Exercise 4.15
#|
Suppose that the procedure halts? exists. If evaluation of the expression
(try try) runs forever, then (halts? try try) returns a true value. This
implies that (try try) halts, which leads to a contradiction. Similarly,
if (try try) returns 'halted, then (halts? try try) returns false. This
implies that (try try) does not halt, which is also a contraction. Therefore
the procedure halts? cannot exist.
|#

;; Exercise 4.16
; b
(define (bifurcate predicate list)
  (define (go list cont)
    (if (null? list)
        (cont (cons '() '()))
        (let ((first (car list))
              (rest (cdr list)))
          (if (predicate first)
              (go rest (lambda (p)
                         (cont (cons (cons first (car p))
                                     (cdr p)))))
              (go rest (lambda (p)
                         (cont (cons (car p)
                                     (cons first (cdr p))))))))))
  (go list identity))

(define (scan-out-defines body)
  (let ((split (bifurcate definition? body)))
    (let ((defines (car split))
          (rest (cdr split)))
      (if (null? defines)
          body
          (list (make-let
                 (map (lambda (d) (list (definition-variable d) ''*unassigned*))
                      defines)
                 (append (map (lambda (d) (list
                                           'set! (definition-variable d)
                                           (definition-value d)))
                              defines)
                         rest)))))))
; c
#|
I believe this is better installed in make-procedure because I can foresee
expansions to the evaluator where procedure-body is called more often than
make-procedure, which makes it more efficient to do the work inside
make-procedure. If this assumption proves incorrect, then it would be better
to install it in procedure-body.
|#

;; Exercise 4.17
#|
The transformed program has an extra frame because the let expression
evaluates an additional lambda procedure, which creates a frame to perform
the procedure. This difference will not change the behaviour of a correctly
structured program because in both cases, the expression <e3> will be evaluated
within (or extended from) the frame containing all the internally defined
variables. Thus <e3> will have access to the variables when needed.
Furthermore, all internally defined variables will be set before being
accessed by <e3> due to the sequential nature of procedure body evaluation.

To achieve the same result without constructing the extra frame, a lambda
procedure could be transformed so that all internally defined variables become
parameters of the procedure, and when called, these parameters receive the
value of *unassigned* and the procedure body begins by setting these variables
to the proper values.
To illustrate, the procedure
(lambda <vars>
  (define u <e1>)
  (define v <e2>)
  <e3>)

would be transformed into
(lambda (<vars> u v)
  (set! u <e1>)
  (set! v <e2>)
  <e3>)

And whenever this procedure is called, the evaluator would have to add the
additional arguments of *unassigned* for both u and v.
|#

;; Exercise 4.18
#|
The procedure bound to solve using the strategy proposed in the exercise is:
(lambda (f y0 dt)
  (let ((y *unassigned*)
        (dy *unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b))
    y))
This procedure will not work; there will be no issue in evaluating the
argument that will be bound to a because although the value of dy at that
time will be *unassigned*, its access is deferred through use of the delay
special form. The problem occurs when the evaluator evaluates the value for b;
it will perform a lookup for the value of y. At this stage in evaluation, y
is still bound to the value *unassigned*, which will signal an error in the
lookup-variable-value procedure.
This problem does not occur using the strategy proposed in the text, since
when the expression (set! dy (stream-map f y)) is evaluated, the value of y
has already been set to (integral (delay dy) y0 dt)) which is a well-defined
value in the environment.
|#

;; Exercise 4.19
#|
I can see a case for and against each argument:
Ben: based on the structure of the example code, it appears that this
interpretation is the intended behaviour (or else why would the author
place the definition of a after it is used?). However, it is still possible
that this is not the case. Since there is confusion in the intent, I would
say that this example illustrates an odious programming style and should not
be accounted for.

Eva: I agree with Eva in principle because this would be the technical
definition of the simultaneous scope rule. However, as mentioned in the
footnote, implementing the rule is difficult: the issue in the example
occurs because of the order in which the variable values are evaluated.
A possible mechanism to fix this would be to search all internal definition
value expressions for variables that are internally defined. When placing
the corresponding set! procedures to give each variable its value, they will
be ordered such that any value expression that references another internally
defined variable will be placed after that variable's set! procedure.
Unfortunately, this will fail when the definitions are circular.

Alyssa: For the reasons discussed above, I agree with Alyssa's view for
practical considerations. Since this programming style is most likely odious,
I think it is better to point the programmer to this rather than to return a
'valid' value.
|#

;; Exercise 4.20
;a
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-bindings exp) (cadr exp))
(define (letrec-parameters exp)
  (map car (letrec-bindings exp)))
(define (letrec-arguments exp)
  (map cadr (letrec-bindings exp)))
(define (letrec-body exp) (cddr exp))
(define (letrec->let exp)
  (let ((vars (letrec-parameters exp))
        (vals (letrec-arguments exp)))
    (let ((new-bindings (map (lambda (v) (list v ''*unassigned*))
                             vars))
          (new-body (append
                     (map (lambda (var val) (list 'set! var val))
                          vars vals)
                     (letrec-body exp))))
      (make-let new-bindings new-body))))
;b
#|
When using letrec, the procedures for even? and odd? are evaluated in the
frame generated by the letrec: thus each procedure's associated environment
contains the necessary variables for lookup.
If letrec is replaced by let, the procedures must be evaluated BEFORE the
let frame is created. Thus each procedure's associated environment is the
same environment as f. That is, the procedures are bound to the let
expression's enclosing environment. This means that the procedures will
fail to lookup the necessary variables (ie. even? and odd?) and produce
an error.
|#

;; Exercise 4.21
;a
(define (fibonacci n)
  ((lambda (n)
     ((lambda (fib)
        (fib fib n))
      (lambda (fb k)
        (cond ((= k 0) 0)
              ((= k 1) 1)
              (else (+ (fb fb (- k 1))
                       (fb fb (- k 2))))))))
   n))

;b
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
