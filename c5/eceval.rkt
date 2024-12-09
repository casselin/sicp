#lang sicp
(#%require "registers.rkt"
           "primitives.rkt"
           "compiler.rkt")
(#%provide (all-defined))
;;; Section 5.4 The Explicit-Control Evaluator
;; Controller specification
(define eceval-controller
  '(controller
      (assign compapp (label compound-apply))
      (branch (label external-entry))     ; branches if flag is set
    read-eval-print-loop
      (perform (op initialize-stack))
      (perform
       (op prompt-for-input) (const ";;; EC-Eval input:"))
      (assign exp (op read))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (goto (label eval-dispatch))
    print-result
      (perform (op print-stack-statistics)) ; for monitoring performance
      (perform
       (op announce-output) (const ";;; EC-Eval value:"))
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))
    eval-dispatch
      (test (op self-evaluating?) (reg exp))
      (branch (label ev-self-eval))
      (test (op variable?) (reg exp))
      (branch (label ev-variable))
      (test (op quoted?) (reg exp))
      (branch (label ev-quoted))
      (test (op assignment?) (reg exp))
      (branch (label ev-assignment))
      (test (op definition?) (reg exp))
      (branch (label ev-definition))
      (test (op if?) (reg exp))
      (branch (label ev-if))
      (test (op lambda?) (reg exp))
      (branch (label ev-lambda))
      (test (op begin?) (reg exp))
      (branch (label ev-begin))
      ;; Exercise 5.23/24
      (test (op cond?) (reg exp))
      (branch (label ev-cond))
      (test (op let?) (reg exp))
      (branch (label ev-let))
      ;; Exercise 5.48
      (test (op compile-and-run?) (reg exp))
      (branch (label ev-compile-and-run))
      (test (op application?) (reg exp))
      (branch (label ev-application))
      (goto (label unknown-expression-type))
    ev-self-eval
      (assign val (reg exp))
      (goto (reg continue))
    ev-variable
      (assign val (op lookup-variable-value) (reg exp) (reg env))
      (goto (reg continue))
    ev-quoted
      (assign val (op text-of-quotation) (reg exp))
      (goto (reg continue))
    ev-lambda
      (assign unev (op lambda-parameters) (reg exp))
      (assign exp (op lambda-body) (reg exp))
      (assign val (op make-procedure)
                  (reg unev) (reg exp) (reg env))
      (goto (reg continue))
    ev-application
      (save continue)
      (assign unev (op operands) (reg exp))
      (assign exp (op operator) (reg exp))
      (test (op variable?) (reg exp))
      (branch (label ev-appl-operator-lookup))
      (save env)
      (save unev)
      (assign continue (label ev-appl-did-operator))
      (goto (label eval-dispatch))
    ev-appl-operator-lookup
      (assign continue (label ev-appl-op-after-restore))
      (goto (label eval-dispatch))
    ev-appl-did-operator
      (restore unev)                      ;; the operands
      (restore env)
     ev-appl-op-after-restore
      (assign argl (op empty-arglist))
      (assign proc (reg val))             ;; the operator
      (test (op no-operands?) (reg unev))
      (branch (label apply-dispatch))
      (save proc)
    ev-appl-operand-loop
      (save argl)
      (assign exp (op first-operand) (reg unev))
      (test (op last-operand?) (reg unev))
      (branch (label ev-appl-last-arg))
      (save env)
      (save unev)
      (assign continue (label ev-appl-accumulate-arg))
      (goto (label eval-dispatch))
    ev-appl-accumulate-arg
      (restore unev)
      (restore env)
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (assign unev (op rest-operands) (reg unev))
      (goto (label ev-appl-operand-loop))
    ev-appl-last-arg
      (assign continue (label ev-appl-accum-last-arg))
      (goto (label eval-dispatch))
    ev-appl-accum-last-arg
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (restore proc)
      (goto (label apply-dispatch))
    apply-dispatch
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-apply))
      (test (op compound-procedure?) (reg proc))
      (branch (label compound-apply))
      (test (op compiled-procedure?) (reg proc))
      (branch (label compiled-apply))
      (goto (label unknown-procedure-type))
    primitive-apply
      (assign val (op apply-primitive-procedure)
                  (reg proc)
                  (reg argl))
      (restore continue)
      (goto (reg continue))
    compound-apply
      (assign unev (op procedure-parameters) (reg proc))
      (assign env (op procedure-environment) (reg proc))
      (assign env (op extend-environment)
                  (reg unev) (reg argl) (reg env))
      (assign unev (op procedure-body) (reg proc))
      (goto (label ev-sequence))
    compiled-apply
      (restore continue)
      (assign val (op compiled-procedure-entry) (reg proc))
      (goto (reg val))
    ev-begin
      (assign unev (op begin-actions) (reg exp))
      (save continue)
      (goto (label ev-sequence))
    ;; tail-recursive ev-sequence
    ev-sequence
      (assign exp (op first-exp) (reg unev))
      (test (op last-exp?) (reg unev))
      (branch (label ev-sequence-last-exp))
      (save unev)
      (save env)
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))
    ev-sequence-continue
      (restore env)
      (restore unev)
      (assign unev (op rest-exps) (reg unev))
      (goto (label ev-sequence))
    ev-sequence-last-exp
      (restore continue)
      (goto (label eval-dispatch))
    ;
    ;; non-tail recursive ev-sequence
    #|
    ev-sequence
      (test (op no-more-exps?) (reg unev))
      (branch (label ev-sequence-end))
      (assign exp (op first-exp) (reg unev))
      (save unev)
      (save env)
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))
    ev-sequence-continue
      (restore env)
      (restore unev)
      (assign unev (op rest-exps) (reg unev))
      (goto (label ev-sequence))
    ev-sequence-end
      (restore continue)
      (goto (reg continue))
    |#
    ev-if
      (save exp)                                ;; save expression for later
      (save env)
      (save continue)
      (assign continue (label ev-if-decide))
      (assign exp (op if-predicate) (reg exp))
      (goto (label eval-dispatch))              ;; evaluate the predicate
    ev-if-decide
      (restore continue)
      (restore env)
      (restore exp)
      (test (op true?) (reg val))
      (branch (label ev-if-consequent))
    ev-if-alternative
      (assign exp (op if-alternative) (reg exp))
      (goto (label eval-dispatch))
    ev-if-consequent
      (assign exp (op if-consequent) (reg exp))
      (goto (label eval-dispatch))
    ev-assignment
      (assign unev (op assignment-variable) (reg exp))
      (save unev)                             ;; save variable for later
      (assign exp (op assignment-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-assignment-1))
      (goto (label eval-dispatch))            ;; evaluate the assignment value
    ev-assignment-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform
       (op set-variable-value!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))
    ev-definition
      (assign unev (op definition-variable) (reg exp))
      (save unev)
      (assign exp (op definition-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-definition-1))
      (goto (label eval-dispatch))
    ev-definition-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform
       (op define-variable!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))
    ;; Exercise 5.23
    #|
    ev-cond-derived
      (assign exp (op cond->if) (reg exp))
      (goto (label ev-if))
    |#
    ev-let
      (assign exp (op let->combination) (reg exp))
      (goto (label ev-application))
    ;; Exercise 5.24
    ev-cond
      (assign unev (op cond-clauses) (reg exp))
      (save continue)
      (goto (label ev-cond-loop))
    ev-cond-loop
      (test (op no-clauses?) (reg unev))
      (branch (label ev-cond-end))
      (assign exp (op first-clause) (reg unev))
      (test (op cond-else-clause?) (reg exp))
      (branch (label ev-cond-true))
      (save exp)
      (save unev)
      (save env)
      (assign continue (label ev-cond-decide))
      (assign exp (op cond-predicate) (reg exp))
      (goto (label eval-dispatch))
    ev-cond-decide
      (restore env)
      (restore unev)
      (restore exp)
      (test (op true?) (reg val))
      (branch (label ev-cond-true))
      (assign unev (op rest-clauses) (reg unev))
      (goto (label ev-cond-loop))
    ev-cond-true
      (assign unev (op cond-actions) (reg exp))
      (goto (label ev-sequence))
    ev-cond-end
      (assign val (const false))
      (restore continue)
      (goto (reg continue))
    ev-compile-and-run
      (assign exp (op compile-and-run-exp) (reg exp))
      (assign val (op compile-and-run) (reg exp))
      (goto (label external-entry))
    unknown-expression-type
      (assign val (const unknown-expression-type-error))
      (goto (label signal-error))
    unknown-procedure-type
      (restore continue)      ; clean up stack (from apply-dispatch)
      (assign val (const unknown-procedure-type-error))
      (goto (label signal-error))
    external-entry
      (perform (op initialize-stack))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (goto (reg val))
    signal-error
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))
    done))

(define (compile-and-run expression)
  (assemble (statements
             (compile expression 'val 'return '()))
            eceval))

(define eceval-operations
  (append primitive-procedures
          (list ;;primitive operations
           (list 'read read)
           ;;syntax procedures
           (list 'self-evaluating? self-evaluating?)
           (list 'quoted? quoted?)
           (list 'text-of-quotation text-of-quotation)
           (list 'variable? variable?)
           (list 'assignment? assignment?)
           (list 'assignment-variable assignment-variable)
           (list 'assignment-value assignment-value)
           (list 'definition? definition?)
           (list 'definition-variable definition-variable)
           (list 'definition-value definition-value)
           (list 'lambda? lambda?)
           (list 'lambda-parameters lambda-parameters)
           (list 'lambda-body lambda-body)
           (list 'if? if?)
           (list 'if-predicate if-predicate)
           (list 'if-consequent if-consequent)
           (list 'if-alternative if-alternative)
           (list 'cond? cond?)
           (list 'cond-clauses cond-clauses)
           (list 'cond-else-clause? cond-else-clause?)
           (list 'cond-predicate cond-predicate)
           (list 'cond-actions cond-actions)
           (list 'cond->if cond->if)
           (list 'first-clause first-clause)
           (list 'rest-clauses rest-clauses)
           (list 'no-clauses? no-clauses?)
           (list 'let? let?)
           (list 'let->combination let->combination)
           (list 'begin? begin?)
           (list 'begin-actions begin-actions)
           (list 'last-exp? last-exp?)
           (list 'first-exp first-exp)
           (list 'rest-exps rest-exps)
           (list 'application? application?)
           (list 'operator operator)
           (list 'operands operands)
           (list 'no-operands? no-operands?)
           (list 'first-operand first-operand)
           (list 'rest-operands rest-operands)
           (list 'compile-and-run? compile-and-run?)
           (list 'compile-and-run-exp compile-and-run-exp)
           ;;operations
           (list 'true? true?)
           (list 'make-procedure make-procedure)
           (list 'compound-procedure? compound-procedure?)
           (list 'procedure-parameters procedure-parameters)
           (list 'procedure-body procedure-body)
           (list 'procedure-environment procedure-environment)
           (list 'make-compiled-procedure make-compiled-procedure)
           (list 'compiled-procedure? compiled-procedure?)
           (list 'compiled-procedure-entry compiled-procedure-entry)
           (list 'compiled-procedure-env compiled-procedure-env)
           (list 'extend-environment extend-environment)
           (list 'lookup-variable-value lookup-variable-value)
           (list 'set-variable-value! set-variable-value!)
           (list 'lexical-address-lookup lexical-address-lookup)
           (list 'lexical-address-set! lexical-address-set!)
           (list 'define-variable! define-variable!)
           (list 'apply apply)
           (list 'primitive-procedure? primitive-procedure?)
           (list 'apply-primitive-procedure apply-primitive-procedure)
           (list 'prompt-for-input prompt-for-input)
           (list 'announce-output announce-output)
           (list 'user-print user-print)
           (list 'empty-arglist empty-arglist)
           (list 'adjoin-arg adjoin-arg)
           (list 'last-operand? last-operand?)
           (list 'no-more-exps? no-more-exps?) ;for non-tail-recursive machine
           (list 'compile-and-run compile-and-run)
           ;;lazy primitives
           (list 'delay-it delay-it)
           (list 'thunk? thunk?)
           (list 'thunk-exp thunk-exp)
           (list 'thunk-env thunk-env)
           (list 'evaluated-thunk? evaluated-thunk?)
           (list 'thunk-value thunk-value)
           (list 'memoize-thunk! memoize-thunk!)
           (list 'get-global-environment get-global-environment))))

(define eceval
  (make-machine
   eceval-operations
   eceval-controller))

(define (start-eceval)
  (set-global-env! (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))

(define (compile-and-go expression)
  (let ((instructions
         (assemble (statements
                    (compile expression 'val 'return '()))
                   eceval)))
    (set-global-env! (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))