#lang sicp
;;; Designing Register Machines
;; GCD machine
(define gcd-controller
  '(controller
    test-b
      (test (op =) (reg b) (const 0))
      (branch (label gcd-done))
      (assign t (op rem) (reg a) (reg b))
      (assign a (reg b))
      (assign b (reg t))
      (goto (label test-b))
    gcd-done))

;; Exercise 5.2
; Seperate data-path controllers
#|
(data-paths
 (registers
  ((name n))
  ((name p)
   (buttons ((name p<-1) (source (constant 1)))
            ((name p<-t) (source (operation *)))))
  ((name c)
   (buttons ((name c<-1) (source (constant 1)))
            ((name c<-+) (source (operation +))))))

 (operations ((name *)
              (inputs (register p) (register c)))
             ((name +)
              (inputs (register c) (constant 1)))
             ((name >)
              (inputs (register c) (register n)))))

(controller
 (p<-1)
 (c<-1)
 test-c
   (test >)
   (branch (label fact-done))
   (p<-*)
   (c<-+)
   (goto (label test-c))
 fact-done)
|#
; Abridged version
(define fact-iter-controller
  '(controller
    (assign p (const 1))
    (assign c (const 1))
    test-c
      (test (op >) (reg c) (reg n))
      (branch (label fact-done))
      (assign p (op *) (reg p) (reg c))
      (assign c (op +) (reg c) (const 1))
      (goto (label test-c))
    fact-done))

;; Exercise 5.3
; good-enough? and improve as primitives
(define sqrt-controller-1
  '(controller
    (assign guess (const 1.0))
    test-guess
      (test (op good?) (reg guess) (reg x))
      (branch (label sqrt-done))
      (assign guess (op improve) (reg guess) (reg x))
      (goto (label test-guess))
    sqrt-done))

; good-enough? and improve expanded
(define sqrt-controller-2
  '(controller
    (assign guess (const 1.0))
    test-guess
      (assign t (op *) (reg guess) (reg guess))
      (assign t (op -) (reg t) (reg x))
      (test (op <) (reg t) (const 0))
      (branch (label abs-neg))
    abs-pos
      (test (op <) (reg t) (const 0.001))
      (branch (label sqrt-done))
      (assign t (op /) (reg x) (reg guess))
      (assign t (op +) (reg t) (reg guess))
      (assign guess (op /) (reg t) (const 2))
      (goto (label test-guess))
    abs-neg
      (assign t (op *) (const -1) (reg t))
      (goto (label abs-pos))
    sqrt-done))

;; Exercise 5.4
; Recursive exponentiation
(define expt-recur-controller
  '(controller
    (assign continue (label expt-done))     ; set up final return address
    expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label base-case))
      ;; Set up for the recursive call by saving continue.
      ;; Set up continue so that the computation will continue
      ;; at after-expt when the subroutine returns.
      (save continue)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-expt))
      (goto (label expt-loop))
    after-expt
      (restore continue)
      (assign val (op *) (reg b) (reg val)) ; val now contains b(b^(n-1))
      (goto (reg continue))                 ; return to caller
    base-case
      (assign val (const 1))                ; base case: b^0 = 1
      (goto (reg continue))                 ; return to caller
    expt-done))

; Iterative exponentiation
(define expt-iter-controller
  '(controller
    (assign product (const 1))
    expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label expt-done))
      (assign n (op -) (reg n) (const 1))
      (assign product (op *) (reg b) (reg product))
      (goto (label expt-loop))
    expt-done))

;; Exercise 5.6
#|
The restore and save operations on the continue register inside the
afterfib-n-1 label are superfluous. This is because there is no
change to the continue register between these operations, so the
value can be left on the stack unchanged.
|#

;;; A Resgister-Machine Simulator
; by exercise 5.13 make-machine no longer needs a list of register names
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    #|
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    |#
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    ;; exercise 5.12
    (update-data-path-info! controller-text machine)
    ;
    machine))

;; Registers
; tracing added by exercise 5.18
(define (make-register name)
  (let ((contents '*unassigned*)
        (tracing false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if tracing
                   (let ((prev contents))
                     (newline)
                     (display
                      (list name 'old '= contents 'new '= value))))
               (set! contents value)))
             ;(lambda (value) (set! contents value)))
            ((eq? message 'trace-on)
             (set! tracing true)
             (display (list name 'tracing 'on)))
            ((eq? message 'trace-off)
             (set! tracing false)
             (display (list name 'tracing 'off)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

; The stack
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else (error "Unknown request -- STACK"
                         message))))
    dispatch))

(define (pop stack)
  (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

; The basic machine
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        ;; exercise 5.12
        (inst-list '())
        (entry-points '())
        (stack-regs '())
        (reg-sources '())
        ;; exercise 5.15
        (inst-count 0)
        ;; exercise 5.16
        (inst-tracing false))
        ;
    ;; exercise 5.15
    (define (print-instruction-count)
        (newline)
        (display (list 'instructions-executed '= inst-count))
        (set! inst-count 0))
    ;
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))
                 (list 'print-instruction-count
                       (lambda () (print-instruction-count)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      ;; exercise 5.13
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (begin
                (allocate-register name)
                (lookup-register name)))))
      ;
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              ;; exercise 5.16
              (let ((next-inst (car insts)))
                (if inst-tracing
                    (begin
                      ;; exercise 5.17
                      (for-each
                       (lambda (label)
                         (newline)
                         (display "Entering label: ")
                         (display label))
                       (instruction-preceding-labels next-inst))
                      ;
                      (newline)
                      (display (instruction-text next-inst))))
                ((instruction-execution-proc next-inst))
                ;; exercise 5.15
                (set! inst-count (+ 1 inst-count))
                ;
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ;; exercise 5.12
              ((eq? message 'install-instruction-list)
               (lambda (insts) (set! inst-list insts)))
              ((eq? message 'install-entry-points)
               (lambda (entries) (set! entry-points entries)))
              ((eq? message 'install-stack-registers)
               (lambda (registers) (set! stack-regs registers)))
              ((eq? message 'install-register-sources)
               (lambda (sources) (set! reg-sources sources)))
              ((eq? message 'get-instruction-list) inst-list)
              ((eq? message 'get-entry-points) entry-points)
              ((eq? message 'get-stack-registers) stack-regs)
              ((eq? message 'get-register-sources) reg-sources)
              ;; exercise 5.15
              ((eq? message 'print-instruction-count)
               (print-instruction-count))
              ;; exercise 5.16
              ((eq? message 'trace-on)
               (set! inst-tracing true)
               'instruction-tracing-on)
              ((eq? message 'trace-off)
               (set! inst-tracing false)
               'instruction-tracing-off)
              ;; exercise 5.18
              ((eq? message 'register-trace-on)
               (lambda (name)
                 (let ((reg (lookup-register name)))
                   (reg 'trace-on))))
              ((eq? message 'register-trace-off)
               (lambda (name)
                 (let ((reg (lookup-register name)))
                   (reg 'trace-off))))
              ;
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;; The Assembler
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              ;; exercise 5.8
                              (if (has-label? next-inst labels)
                                  (error "Duplicate label -- EXTRACT-LABELS"
                                         next-inst)
                                  ;; exercise 5.17
                                  (begin
                                    (if (not (null? insts))
                                        (let ((first-inst (car insts)))
                                          (set-instruction-preceding-labels!
                                           first-inst (cons next-inst
                                                            (instruction-preceding-labels
                                                             first-inst)))))
                                        (receive insts
                                                 (cons (make-label-entry next-inst
                                                                         insts)
                                                       labels))))
                              ;
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                       labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

#|
(define (make-instruction text)
  (cons text '()))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))
|#

(define (make-instruction text)
  (list text '() '()))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cadr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-car! (cdr inst) proc))
;; exercise 5.17
(define (instruction-preceding-labels inst)
  (caddr inst))
(define (set-instruction-preceding-labels! inst labels)
  (set-car! (cddr inst) labels))
;

(define (make-label-entry label-name insts)
  (cons label-name insts))
(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

;; Exercise 5.8
#|
The contents of register a will be 3. This is because extract-labels
builds the table of labels in the order in which they appear in the
controller text. In addition, looking up a label uses the assoc procedure,
which returns once it finds the first occurrence of the given key. Thus
execution will proceed to the first 'here' label and assign the value of 3
to the a register.
|#
(define (has-label? label-name labels)
  (let ((label (assoc label-name labels)))
    (if label true false)))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                ; exercise 5.9
                (if (label-exp? e)
                    (error "Cannot perform operation on label" e)
                    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

;; Exercise 5.11
; a
#|
The first two instructions after the label afterfib-n-2:
(assign n (reg val))          ; n now contains Fib(n-2)
(restore val)                 ; val now contains Fib(n-1)

can be replaced with the single line
(restore n)

The restored value actually belongs to the register val that represented
Fib(n-1). Instead, Fib(n-1) will be stored in register n instead. Since
register val already contains Fib(n-2) upon entering this label, we may
add these two registers and still assign the correct value to val (since
addition is commutative).
|#

; b
(define (make-save-b inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (make-stack-item (stack-inst-reg-name inst)
                                   (get-contents reg)))
      (advance-pc pc))))

(define (make-restore-b inst machine stack pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (let ((item (pop stack)))
          (if (eq? reg-name (stack-item-name item))
              (begin
                (set-contents! reg (stack-item-value item))
                (advance-pc pc))
              (error "Mismatched registers -- RESTORE"
                     (list reg-name
                           (stack-item-name item)))))))))

(define (make-stack-item reg-name value)
  (list reg-name value))
(define (stack-item-name item)
  (car item))
(define (stack-item-value item)
  (cadr item))

; c
#|
This requires changes to the stack operations in make-new-machine:
let (...
     (stacks (make-stack-table))
     ...)
...
(define (dispatch message)
  (cond (...
         (eq? message 'stacks) stacks)
         ...)))
|#
(define (make-stack-table)
  (let ((stacks '()))
    (define (allocate-stack reg-name)
      (let ((s (assoc reg-name stacks)))
        (if s
            (error "Stack already allocated -- ALLOCATE-STACK" reg-name)
            (set! stacks (cons (cons reg-name
                                     (make-stack))
                               stacks)))))
    (define (push-stack reg-name val)
      (let ((s (assoc reg-name stacks)))
        (if s
            (push (cdr s) val)
            (error "No stack allocated for register -- PUSH-STACKS"
                   reg-name))))
    (define (pop-stack reg-name)
      (let ((s (assoc reg-name stacks)))
        (if s
            (pop (cdr s))
            (error "No stack allocated for register-- POP-STACKS"
                   reg-name))))
    (define (initialize-stacks)
      (for-each (lambda (s)
                  ((cdr s) 'initialize))
                stacks))
    (define (print-stacks)
      (for-each (lambda (s) (display s) (newline))
                stacks))
    (define (dispatch message)
      (cond ((eq? message 'allocate-stack) allocate-stack)
            ((eq? message 'push) push-stack)
            ((eq? message 'pop) pop-stack)
            ((eq? message 'initialize) (initialize-stacks))
            ((eq? message 'print) (print-stacks))
            (else
             (error "Unknown request -- STACKS" message))))
    dispatch))

(define (make-save-c inst machine stacks pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        ((stacks 'push) reg-name (get-contents reg))
        (advance-pc pc)))))
(define (make-restore-c inst machine stacks pc)
  (let ((reg-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine reg-name)))
      (lambda ()
        (set-contents! reg ((stacks 'pop) reg-name))
        (advance-pc pc)))))

;; Exercise 5.12
(define (make-table)
  (list '*table*))
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (append-unique! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (if (not (member value (cdr record)))
            (set-cdr! record (cons value (cdr record))))
        (set-cdr! table
                  (cons (cons key (list value))
                        (cdr table)))))
  'ok)
(define (values table)
  (define (loop kvs)
    (if (null? kvs)
        '()
        (append (cdar kvs) (loop (cdr kvs)))))
  (loop (cdr table)))
(define (filter pred seq)
  (if (null? seq)
      '()
      (if (pred (car seq))
          (cons (car seq) (filter pred (cdr seq)))
          (filter pred (cdr seq)))))

(define (merge-distinct list1 list2)
  ; list1 and list2 are each assumed to contain no duplicates
  (if (null? list1)
      list2
      (let ((first (car list1))
            (rest (cdr list1)))
        (if (member first list2)
            (merge-distinct rest list2)
            (cons first (merge-distinct rest list2))))))

(define (extract-goto-regs goto-list)
  (map (lambda (goto) (register-exp-reg (cadr goto)))
       (filter (lambda (goto) (register-exp? (cadr goto)))
               goto-list)))
(define (extract-stack-regs save-list restore-list)
  (merge-distinct
   (map cadr save-list)
   (map cadr restore-list)))
(define (extract-reg-sources assign-list)
  (let ((reg-table (make-table)))
    (for-each
     (lambda (assign)
       (let ((reg-name (cadr assign))
             (source (cddr assign)))
         (if (and (pair? source) (null? (cdr source))) ;singleton list
             (append-unique! reg-name (car source) reg-table)
             (append-unique! reg-name source reg-table))))
     assign-list)
    (cdr reg-table)))
(define (update-data-path-info! insts machine)
  (let ((group-insts (make-table)))
    (for-each
     (lambda (inst)
       (if (not (symbol? inst))
           (let ((inst-type (car inst)))
             (append-unique! inst-type inst group-insts))))
     insts)
    (let ((goto-list (lookup 'goto group-insts))
          (save-list (lookup 'save group-insts))
          (restore-list (lookup 'restore group-insts))
          (assign-list (lookup 'assign group-insts)))
      ((machine 'install-instruction-list) (values group-insts))
      ((machine 'install-entry-points)
       (extract-goto-regs (if goto-list goto-list '())))
      ((machine 'install-stack-registers)
       (extract-stack-regs (if save-list save-list '())
                           (if restore-list restore-list '())))
      ((machine 'install-register-sources)
       (extract-reg-sources (if assign-list assign-list '()))))))

(define fib-machine-controller
  '(controller
      (assign continue (label fib-done))
    fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate-answer))
      ;; set up to compute Fib(n-1)
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n)                            ;save old value of n
      (assign n (op -) (reg n) (const 1)) ;clobber n to n-1
      (goto (label fib-loop))             ;perform recursive call
    afterfib-n-1                          ;upon return, val contains Fib(n-1)
      (restore n)
      (restore continue)
      ;; set up to compute Fib(n-2)
      (assign n (op -) (reg n) (const 2))
      (save continue)
      (assign continue (label afterfib-n-2))
      (save val)                          ;save Fib(n-1)
      (goto (label fib-loop))
    afterfib-n-2                          ;upon return, val contains Fib(n-2)
      (assign n (reg val))                ;n now contains Fib(n-2)
      (restore val)                       ;val now contains Fib(n-1)
      (restore continue)
      (assign val                         ;Fib(n-1) + Fib(n-2)
              (op +) (reg val) (reg n))
      (goto (reg continue))               ;return to caller, answer is in val
    immediate-answer
      (assign val (reg n))                ;base case: Fib(n) = n
      (goto (reg continue))
    fib-done))

;; Exercise 4.14
(define fact-recur-controller
  '(controller
    start
      (perform (op initialize-stack))
      (assign n (op read))
      (assign continue (label fact-done))
    fact-loop
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      ;; Set up for the recursive call by saving n and continue
      ;; Set up continue so that the computation will continue
      ;; at after-fact when the subroutine returns
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
    after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val)) ;val now contains n(n-1)!
      (goto (reg continue))                 ;return to caller
    base-case
      (assign val (const 1))                ;base case: 1!=1
      (goto (reg continue))                 ;return to caller
    fact-done
      (perform (op print) (reg val))
      (perform (op print-instruction-count))
      (goto (label start))))