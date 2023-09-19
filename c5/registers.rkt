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
(define fact-iter-contoller
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
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

; Registers
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

; The stack
(define (make-stack)
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
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
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
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
                              ; exercise 5.8
                              (if (has-label? next-inst labels)
                                  (error "Duplicate label -- EXTRACT-LABELS"
                                         next-inst)
                                  (receive insts
                                           (cons (make-label-entry next-inst
                                                                   insts)
                                                 labels)))
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

(define (make-instruction text)
  (cons text '()))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

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
