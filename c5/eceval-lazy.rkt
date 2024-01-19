#lang sicp
(#%require "registers.rkt"
           "primitives.rkt")
;;; Exercise 5.25 Lazy Explicit Control Evaluator
;; Controller specification
(define eceval-controller
  '(read-eval-print-loop
      (perform (op initialize-stack))
      (perform
       (op prompt-for-input) (const ";;; EC-Eval-Lazy input:"))
      (assign exp (op read))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (goto (label actual-value))
    print-result
      (perform (op print-stack-statistics)) ; for monitoring performance
      (perform
       (op announce-output) (const ";;; EC-Eval-Lazy value:"))
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
      ;
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
      (save env)
      (assign unev (op operands) (reg exp))
      (save unev)
      (assign exp (op operator) (reg exp))
      (assign continue (label ev-appl-did-operator))
      (goto (label actual-value))
    ev-appl-did-operator
      (restore unev)                      ;; the operands
      (restore env)
      (assign argl (op empty-arglist))
      (assign proc (reg val))             ;; the operator
      (goto (label apply-dispatch))
    apply-dispatch
      (test (op primitive-procedure?) (reg proc))
      (branch (label appl-prim-setup))
      (test (op compound-procedure?) (reg proc))
      (branch (label appl-comp-loop))
      (goto (label unknown-procedure-type))
    appl-prim-setup
      (test (op no-operands?) (reg unev))
      (branch (label primitive-apply))
      (save proc)
    appl-prim-loop
      (save argl)
      (assign exp (op first-operand) (reg unev))
      (test (op last-operand?) (reg unev))
      (branch (label appl-prim-last-arg))
      (save env)
      (save unev)
      (assign continue (label appl-prim-accum-arg))
      (goto (label actual-value))
    appl-prim-accum-arg
      (restore unev)
      (restore env)
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (assign unev (op rest-operands) (reg unev))
      (goto (label appl-prim-loop))
    appl-prim-last-arg
      (assign continue (label appl-prim-accum-last-arg))
      (goto (label actual-value))
    appl-prim-accum-last-arg
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (restore proc)
      (goto (label primitive-apply))
    appl-comp-loop
      (test (op no-operands?) (reg unev))
      (branch (label compound-apply))
      (assign exp (op first-operand) (reg unev))
      (assign exp (op delay-it) (reg exp) (reg env))
      (assign argl (op adjoin-arg) (reg exp) (reg argl))
      (assign unev (op rest-operands) (reg unev))
      (goto (label appl-comp-loop))
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
    ev-begin
      (assign unev (op begin-actions) (reg exp))
      (save continue)
      (goto (label ev-sequence))
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
    ev-if
      (save exp)                                ;; save expression for later
      (save env)
      (save continue)
      (assign continue (label ev-if-decide))
      (assign exp (op if-predicate) (reg exp))
      (goto (label actual-value))               ;; evaluate the predicate
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
    ev-let
      (assign exp (op let->combination) (reg exp))
      (goto (label ev-application))
    ev-cond
      (assign exp (op cond->if) (reg exp))
      (goto (label ev-if))
    actual-value
      (save continue)
      (assign continue (label force-it))
      (goto (label eval-dispatch))
    force-it
      (test (op thunk?) (reg val))
      (branch (label force-thunk))
      (test (op evaluated-thunk?) (reg val))
      (branch (label force-memo))
      (restore continue)
      (goto (reg continue))
    force-thunk
      (assign exp (op thunk-exp) (reg val))
      (assign env (op thunk-env) (reg val))
      (save val)
      (assign continue (label thunk-after-eval))
      (goto (label actual-value))
    thunk-after-eval
      (restore unev) ;data from (save val) in force-thunk
      (perform (op memoize-thunk!) (reg unev) (reg val))
      (restore continue)
      (goto (reg continue))
    force-memo
      (restore continue)
      (assign val (op thunk-value) (reg val))
      (goto (reg continue))
    unknown-expression-type
      (assign val (const unknown-expression-type-error))
      (goto (label signal-error))
    unknown-procedure-type
      (restore continue)      ; clean up stack (from apply-dispatch)
      (assign val (const unknown-procedure-type-error))
      (goto (label signal-error))
    signal-error
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))
    done))

(define eceval
  (make-machine
   eceval-operations
   eceval-controller))