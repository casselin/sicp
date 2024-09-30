#lang sicp
(#%require "primitives.rkt"
           "registers.rkt"
           "eceval.rkt")

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (compile exp target linkage cenv)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage cenv))
        ((assignment? exp)
         (compile-assignment exp target linkage cenv))
        ((definition? exp)
         (compile-definition exp target linkage cenv))
        ((if? exp) (compile-if exp target linkage cenv))
        ((lambda? exp) (compile-lambda exp target linkage cenv))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
                           cenv))
        ((cond? exp) (compile (cond->if exp) target linkage cenv))
        ((let? exp) (compile (let->combination exp) target linkage cenv))
        ((open-code? exp cenv) (compile-open-code exp target linkage cenv))
        ((application? exp)
         (compile-application exp target linkage cenv))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
                                    '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
                                    `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '()
    (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

#| Replaced in exercise 5.42
(define (compile-variable exp target linkage cenv)
  (end-with-linkage
   linkage
   (make-instruction-sequence
    '(env) (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))))
|#

(define (compile-variable exp target linkage cenv)
  (let ((lex-addr (find-variable exp cenv)))
    (end-with-linkage
     linkage
     (if (eq? lex-addr 'not-found)
         (make-instruction-sequence
          '() `(env ,target)
          `((assign env (op get-global-environment))
            (assign ,target
                    (op lookup-variable-value)
                    (const ,exp)
                    (reg env))))
         (make-instruction-sequence
          '(env) (list target)
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,lex-addr)
                    (reg env))))))))

#| Replaced in Exercise 5.42
(define (compile-assignment exp target linkage cenv)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next cenv)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code
                 (make-instruction-sequence
                  '(env val) (list target)
                  `((perform (op set-variable-value!)
                             (const ,var)
                             (reg val)
                             (reg env))
                    (assign ,target (const ok))))))))
|#

(define (compile-assignment exp target linkage cenv)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next cenv)))
    (let ((lex-addr (find-variable var cenv)))
      (end-with-linkage
       linkage
       (if (eq? lex-addr 'not-found)
           (append-instruction-sequences
            get-value-code
            (make-instruction-sequence
             '(val) `(env ,target)
             `((assign env (op get-global-environment))
               (perform (op set-variable-value!)
                        (const ,var)
                        (reg val)
                        (reg env))
               (assign ,target (const ok)))))
           (preserving
            '(env)
            get-value-code
            (make-instruction-sequence
             '(env val) (list target)
             `((perform (op lexical-address-set!)
                        (const ,lex-addr)
                        (reg val)
                        (reg env))
               (assign ,target (const ok))))))))))

(define (compile-definition exp target linkage cenv)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next cenv)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code
                 (make-instruction-sequence
                  '(env val) (list target)
                  `((perform (op define-variable!)
                             (const ,var)
                             (reg val)
                             (reg env))
                    (assign ,target (const ok))))))))

(define (compile-if exp target linkage cenv)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next cenv))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage cenv))
            (a-code
             (compile (if-alternative exp) target linkage cenv)))
        (preserving
         '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
                                     `((test (op false?) (reg val))
                                       (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

;; Make-label
(define label-counter 0)
(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)
(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-label-number)))))

(define (compile-sequence seq target linkage cenv)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage cenv)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next cenv)
                  (compile-sequence (rest-exps seq) target linkage cenv))))

(define (compile-lambda exp target linkage cenv)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence
          '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry cenv))
       after-lambda))))

;; Operations for compiled procedures
#|
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))
|#

(define (compile-lambda-body exp proc-entry cenv)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (scan-out-defines
                        (lambda-body exp))
                        'val 'return
                        (extend-cenv formals cenv)))))

(define (compile-application exp target linkage cenv)
  (let ((proc-code (compile (operator exp) 'proc 'next cenv))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next cenv))
              (operands exp))))
    (preserving
     '(env continue)
     proc-code
     (preserving
      '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence
         '() '(argl) '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving
               '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving
          '(argl)
          (car operand-codes)
          (make-instruction-sequence
           '(val argl) '(argl)
           '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving
         '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
                           (make-instruction-sequence
                            '(proc argl)
                            (list target)
                            `((assign ,target
                                      (op apply-primitive-procedure)
                                      (reg proc)
                                      (reg argl)))))))
       after-call))))

(define all-regs '(env proc val argl continue arg1 arg2))
(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage))
            (assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                      (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue) all-regs
          `((assign val (op compiled-procedure-entry)
                    (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

(define (registers-needed s)
  (if (symbol? s) '() (car s)))
(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))
(define (statements s)
  (if (symbol? s) (list s) (caddr s)))
(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))
(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        (make-instruction-sequence
                         (list-union (list first-reg)
                                     (registers-needed seq1))
                         (list-difference (registers-modified seq1)
                                          (list first-reg))
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

;; Exercise 5.38
(define (open-code? exp cenv)
  (and (or (tagged-list? exp '+)
           (tagged-list? exp '-)
           (tagged-list? exp '*)
           (tagged-list? exp '=))
       (eq? 'not-found (find-variable (car exp) cenv))))
(define (binary-primitive? exp)
  (or (tagged-list? exp '-)
      (tagged-list? exp '=)))
;a
(define (spread-arguments operands cenv)
  (if (= (length operands) 2)
      (preserving
       '(env)
       (compile (car operands) 'arg1 'next cenv)
       (preserving
        '(arg1)
        (compile (cadr operands) 'arg2 'next cenv)
        (make-instruction-sequence
         '(arg1) '() '())))
      (error
       "Illegal number of operands (should be 2) -- SPREAD-ARGUMENTS"
       operands)))
(define (compile-open-code exp target linkage cenv)
  (cond ((binary-primitive? exp)
         (compile-open-binary exp target linkage cenv))
        (else
         (compile-open-nary exp target linkage cenv))))
;b
(define (compile-open-binary exp target linkage cenv)
  (let ((op (car exp))
        (args (cdr exp)))
    (end-with-linkage
     linkage
     (append-instruction-sequences
      (spread-arguments args cenv)
      (make-instruction-sequence
       '(arg1 arg2) (list target)
       `((assign ,target (op ,op) (reg arg1) (reg arg2))))))))
;d
(define (take n list)
  (if (or (null? list) (= n 0))
      '()
      (cons (car list)
            (take (- n 1) (cdr list)))))
(define (drop n list)
  (cond ((null? list) '())
        ((= n 0) list)
        (else
         (drop (- n 1) (cdr list)))))
(define (halve list)
  (let ((n (quotient (length list) 2)))
    (cons (take n list)
          (drop n list))))
(define (compile-open-nary exp target linkage cenv)
  (let ((op (car exp))
        (args (cdr exp)))
    (let ((id (cond ((eq? op '+) 0)
                    ((eq? op '*) 1)
                    (else (error "Unknown n-ary operator" op)))))
      (cond ((null? args)
             (end-with-linkage
              linkage
              (make-instruction-sequence
               '() (list target)
               `((assign target (const ,id))))))
            ((null? (cdr args))
             (end-with-linkage
              linkage
              (append-instruction-sequences
               (compile (car args) 'arg1 'next cenv)
               (make-instruction-sequence
                '(arg1) (list target)
                `((assign target (op ,op) (reg arg1) (const ,id)))))))
            ((null? (cddr args))
             (end-with-linkage
              linkage
              (append-instruction-sequences
               (spread-arguments args cenv)
               (make-instruction-sequence
                '(arg1 arg2) (list target)
                `((assign ,target (op ,op) (reg arg1) (reg arg2)))))))
            (else
             (let ((split-args (halve args)))
               (end-with-linkage
                linkage
                (append-instruction-sequences
                 (spread-arguments (list (cons op (car split-args))
                                         (cons op (cdr split-args)))
                                   cenv)
                 (make-instruction-sequence
                  '(arg1 arg2) (list target)
                  `((assign ,target (op ,op) (reg arg1) (reg arg2))))))))))))

;; Exercise 5.39
#| moved to primitives.rkt
(define (make-lexical-address frame-num displacement-num)
  (list frame-num displacement-num))
(define (lexical-frame-num lexical-address)
  (car lexical-address))
(define (lexical-displacement-num lexical-address)
  (cadr lexical-address))
(define (env-ref env n)
  (cond ((eq? env the-empty-environment)
         (error "Invalid index -- ENVIRONMENT-REF"
                (list n env)))
        ((= n 0)
         (first-frame env))
        (else
         (env-ref (enclosing-environment env)
                  (- n 1)))))
(define (frame-ref frame n)
  (define (iter vars vals i)
    (if (= i 0)
        (cons (car vars) (car vals))
        (iter (cdr vars) (cdr vals) (- i 1))))
  (iter (frame-variables frame)
        (frame-values frame)
        n))
(define (frame-set! frame val n)
  (define (iter values i)
    (if (= i 0)
        (set-car! values val)
        (iter (cdr values) (- i 1))))
  (iter (frame-values frame) n))

(define (lexical-address-lookup address env)
  (let ((env-index (lexical-frame-num address))
        (frame-index (lexical-displacement-num address)))
    (let ((frame (env-ref env env-index)))
      (let ((binding (frame-ref frame frame-index)))
        (if (eq? (cdr binding) '*unassigned*)
            (error "Unbound variable -- LEXICAL-ADDRESS-LOOKUP" binding)
            (cdr binding))))))

(define (lexical-address-set! addr val env)
  (let ((env-index (lexical-frame-num addr))
        (frame-index (lexical-displacement-num addr)))
    (let ((frame (env-ref env env-index)))
      (frame-set! frame val frame-index)
      'ok)))
|#

;; Exercise 5.40
(define (extend-cenv vars base-cenv)
  (cons vars base-cenv))

;; Exercise 5.41
(define (find-variable var cenv)
  (define (env-loop i env)
    (define (scan j v vars)
      (cond ((null? vars)
             (env-loop (+ i 1) (enclosing-environment env)))
            ((eq? v (car vars))
             (make-lexical-address i j))
            (else
             (scan (+ j 1) v (cdr vars)))))
    (if (eq? env the-empty-environment)
        'not-found
        (scan 0 var (first-frame env))))
  (env-loop 0 cenv))

;; Exercise 5.43
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
(define (make-let bindings body)
  (cons 'let (cons bindings body)))
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

(define (compile-and-go expression)
  (let ((instructions
         (assemble (statements
                    (compile expression 'val 'return '()))
                   eceval)))
    (set-global-env! (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))