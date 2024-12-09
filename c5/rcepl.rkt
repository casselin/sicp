#lang sicp
(#%require "registers.rkt"
           "primitives.rkt"
           "compiler.rkt")

#|
This machine could be made more efficient by reverting the
compiler to a version that does not handle interpreted code.
|#

(define (compile-assemble expression)
  (assemble (statements
             (compile expression 'val 'return '()))
            rcepl))

(define rcepl-controller
  '(controller
    read-compile-exec-print-loop
      (perform (op initialize-stack))
      (perform
       (op prompt-for-input) (const ";;; RCEPL input:"))
      (assign exp (op read))
      (assign val (op compile-assemble) (reg exp))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (goto (reg val))
    print-result
      (perform (op print-stack-statistics)) ; for monitoring performance
      (perform
       (op announce-output) (const ";;; RCEPL value:"))
      (perform (op user-print) (reg val))
      (goto (label read-compile-exec-print-loop))))

(define rcepl-operations
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
           (list 'primitive-procedure? primitive-procedure?)
           (list 'apply-primitive-procedure apply-primitive-procedure)
           (list 'prompt-for-input prompt-for-input)
           (list 'announce-output announce-output)
           (list 'user-print user-print)
           (list 'empty-arglist empty-arglist)
           (list 'adjoin-arg adjoin-arg)
           (list 'last-operand? last-operand?)
           (list 'no-more-exps? no-more-exps?) ;for non-tail-recursive machine
           (list 'compile-assemble compile-assemble)
           ;;lazy primitives
           (list 'delay-it delay-it)
           (list 'thunk? thunk?)
           (list 'thunk-exp thunk-exp)
           (list 'thunk-env thunk-env)
           (list 'evaluated-thunk? evaluated-thunk?)
           (list 'thunk-value thunk-value)
           (list 'memoize-thunk! memoize-thunk!)
           (list 'get-global-environment get-global-environment))))

(define rcepl
  (make-machine
   rcepl-operations
   rcepl-controller))

(define (start-rcepl)
  (rcepl 'start))