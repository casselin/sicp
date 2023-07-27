#lang sicp
;;; Amb and Search

(define (require p)
  (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;; Exercise 4.35
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;; Exercise 4.36
#|
Since i j k and selected in succession, backtracking will only change
the choice of k, which would be unbounded and never produce a failed choice.
This results in an infinite loop.
This problem can be fixed by reversing the order of the choices for i,j, and k.
|#
(define (a-pythagorean-triple)
  (let ((k (an-integer-starting-from 1)))
    (let ((j (an-integer-between 1 k)))
      (let ((i (an-integer-between 1 j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;; Exercise 4.37
#|
This is indeed a faster version, since the search for a suitable integer k
does not take place.
|#

;; Exercise 4.38
(define (distinct? items)
    (cond ((null? items) true)
          ((null? (cdr items)) true)
          ((member (car items) (cdr items)) false)
          (else (distinct? (cdr items)))))

(define (multiple-dwelling)
    (let ((baker (amb 1 2 3 4 5))
          (cooper (amb 1 2 3 4 5))
          (fletcher (amb 1 2 3 4 5))
          (miller (amb 1 2 3 4 5))
          (smith (amb 1 2 3 4 5)))
      (require
        (distinct? (list baker cooper fletcher miller smith)))
      (require (not (= baker 5)))
      (require (not (= cooper 1)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (require (> miller cooper))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (list (list 'baker baker)
            (list 'cooper cooper)
            (list 'fletcher fletcher)
            (list 'miller miller)
            (list 'smith smith))))
#|
There are 5 solutions to the modified puzzle, as one can type try-again 4
times before a fail state is reached.
|#

;; Exercise 4.39
#|
The order of the restrictions should not matter, since the conditions are all
combined with the logical AND operator. I believe the order of the restrictions
does impact the time to find an answer. Currently, the distinct? condition is
performed first, but takes the longest to compute. It is possible to filter
out some of the choices using any of the cheaper requirements first. Thus in
many cases the program won't perform the most expensive requirements check.
A faster version would then be:
|#
(define (faster-multiple-dwelling)
    (let ((baker (amb 1 2 3 4 5))
          (cooper (amb 1 2 3 4 5))
          (fletcher (amb 1 2 3 4 5))
          (miller (amb 1 2 3 4 5))
          (smith (amb 1 2 3 4 5)))
      (require (not (= baker 5)))
      (require (not (= cooper 1)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (require (> miller cooper))
      (require (not (= (abs (- fletcher smith)) 1)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (require
        (distinct? (list baker cooper fletcher miller smith)))
      (list (list 'baker baker)
            (list 'cooper cooper)
            (list 'fletcher fletcher)
            (list 'miller miller)
            (list 'smith smith))))

;; Exercise 4.40
#|
Before the distinct requirement, there at 5^5 sets of assignments. After
eliminating those that are not distinct, 5! sets remain.
|#
(define (fastest-multiple-dwelling)
  (let ((fletcher (amb 1 2 3 4 5)))
    (require (not (= fletcher 1)))
    (require (not (= fletcher 5)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (require (not (<= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= smith cooper)))
        (require (not (<= (abs (- fletcher smith)) 1)))
        (let ((miller (amb 1 2 3 4 5)))
          (require (not (= miller fletcher)))
          (require (not (= miller smith)))
          (require (> miller cooper))
          (let ((baker (amb 1 2 3 4 5)))
            (require (not (= baker 5)))
            (require (not (= baker fletcher)))
            (require (not (= baker cooper)))
            (require (not (= baker smith)))
            (require (not (= baker miller)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))

;; Exercise 4.41
(define (filter pred sequence)
  (cond ((null? sequence) '())
        ((pred (car sequence))
         (cons (car sequence)
               (filter pred (cdr sequence))))
        (else (filter pred (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (permutations s)
  (if (null? s)
      '(())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (filter (lambda (y) (not (equal? x y)))
                                            s))))
               s)))

(define (multiple-dwelling-no-amb)
  (define (requirements? tenants)
    (let ((baker (car tenants))
          (cooper (cadr tenants))
          (fletcher (caddr tenants))
          (miller (cadddr tenants))
          (smith (car (cddddr tenants))))
      (and (not (= baker 5))
           (not (= cooper 1))
           (not (= fletcher 1))
           (not (= fletcher 5))
           (> miller cooper)
           (not (= (abs (- fletcher smith)) 1))
           (not (= (abs (- fletcher cooper)) 1)))))
  (map list '(baker cooper fletcher miller smith)
       (car (filter requirements? (permutations '(1 2 3 4 5))))))

;; Exercise 4.42
#|
Can also be done using amb on the requirements, but interplay between Kitty
and Mary's statements gets tricky
|#
(define (xor p q)
  (or (and p (not q))
      (and (not p) q)))
(define (liars-puzzle)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

;; Exercise 4.43
(define (yacht-puzzle)
  (define fathers '(*fathers* mr-moore col-downing mr-hall sir-barnacle dr-parker))
  (define yachts '(*yachts* lorna melissa rosalind gabrielle mary-ann))
  (let ((melissa (list 'melissa 4))
        (mary-ann (list 'mary-ann (amb 1 2 3 4 5)))) ;modified by exercise
    (require (not (= (cadr mary-ann) (cadr melissa))))
    (let ((lorna (list 'lorna (amb 1 2 3 4 5))))
      (require (not (= (cadr lorna) (cadr melissa))))
      (require (not (= (cadr lorna) (cadr mary-ann))))
      (require (not (= (cadr lorna) 1)))
      (let ((rosalind (list 'rosalind (amb 1 2 3 4 5))))
        (require (not (= (cadr rosalind) (cadr melissa))))
        (require (not (= (cadr rosalind) (cadr mary-ann))))
        (require (not (= (cadr rosalind) (cadr lorna))))
        (require (not (= (cadr rosalind) 3)))
        (let  ((gabrielle (list 'gabrielle (amb 1 2 3 4 5))))
          (require (not (= (cadr gabrielle) (cadr melissa))))
          (require (not (= (cadr gabrielle) (cadr mary-ann))))
          (require (not (= (cadr gabrielle) (cadr lorna))))
          (require (not (= (cadr gabrielle) (cadr rosalind))))
          (require (equal?
                    (list-ref yachts (cadr gabrielle))
                    (caar (filter
                           (lambda (d) (= (cadr d) 5))
                           (list mary-ann gabrielle lorna rosalind melissa)))))
          (list-ref fathers (cadr lorna)))))))
#|
Lorna's father is Colonel Downing. If you ignore Mary Ann's last name there
are 2 solutions: Colonel Downing, and Dr. Parker.
|#

;; Exercise 4.44
(define (make-pos row col)
  (cons row col))
(define (pos-row pos)
  (car pos))
(define (pos-col pos)
  (cdr pos))
(define empty-board '())
(define (adjoin-position row col positions)
  (cons (make-pos row col) positions))
(define (safe? col positions)
  (let ((row (pos-row (car positions)))
        (rest (cdr positions))) ; first position is the new queen
    (accumulate (lambda (x y)
                  (and (not (= row (pos-row x)))
                       (not (= (abs (- row (pos-row x)))
                               (abs (- col (pos-col x)))))
                       y))
                true
                rest)))
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        empty-board
        (let ((rest-queens (queen-cols (- k 1)))
              (new-row (an-integer-between 1 board-size)))
          (let ((new-board (adjoin-position new-row k rest-queens)))
            (require (safe? k new-board))
            new-board))))
  (queen-cols board-size))

;; Exercise 4.46
#|
If the operands were not evaluated from left to right, calls to parse-word
(through the procedures parse-noun-phrase or parse-verb-phrase) would occur
in an unintended order. For instance, to evaluate parse-noun-phrase,
(parse-word nouns) would be evaluated before (parse-word articles), which
would result in parsing failure since, presumably, the next word in the
*unparsed* list would be an article and not a noun.
|#

;; Exercise 4.47
#|
Louis' version of parse-verb-phrase will get stuck in an infinite loop in
the case where the next word to be parsed is not a verb. If the next word
is not a verb, this will cause a failure condition in the amb expression
inside parse-verb-phrase. This triggers the selection of the second
argument. However, the second argument calls parse-verb-phrase again,
causing another amb expression that attempts to parse a verb. This process
will repeat infinitely.
Changing the order of the expressions inside the amb will not change the
behaviour, it will still enter an infinite loop. 
|#

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

#|
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))
|#
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

#| redefined in exercise 4.49 to generate words
(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))
|#

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

;; Exercise 4.48
(define conjunctions '(conjunction but and or yet))
(define adjectives '(smart boring adorable))

(define (parse-simple-noun-phrase)
  (let ((article (parse-word articles)))
    (amb (list 'simple-noun-phrase
               article
               (parse-word nouns))
         (list 'adjective-noun-phrase
               article
               (parse-word adjectives)
               (parse-word nouns)))))

(define (parse-simple-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-sentence)
  (define (maybe-extend sentence)
    (amb sentence
         (maybe-extend (list 'compound-sentence
                             sentence
                             (parse-word conjunctions)
                             (parse-simple-sentence)))))
  (maybe-extend (parse-simple-sentence)))

;; Exercise 4.49
(define (parse-word word-list)
  (define (choose-word words)
    (require (not (null? words)))
    (amb (car words) (choose-word (cdr words))))
  (require (not (null? *unparsed*)))
  (set! *unparsed* (cdr *unparsed*))
  (list (car word-list) (choose-word (cdr word-list))))
#|
(sentence (simple-noun-phrase (article the) (noun student)) (verb studies))
(sentence (simple-noun-phrase (article the) (noun student)) (verb lectures))
(sentence (simple-noun-phrase (article the) (noun student)) (verb eats))
(sentence (simple-noun-phrase (article the) (noun student)) (verb sleeps))
|#

;;; Implementing the Amb Evaluator
(define apply-in-underlying-scheme apply)

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

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
      (make-lambda (cdadr exp)
                   (cddr exp))))

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

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
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

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

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
        (list 'eq? eq?)
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

;;; Structure of the Evaluator
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((amb? exp) (analyze-amb exp))
        ;;exercise 4.50
        ((ramb? exp) (analyze-ramb exp))
        ;;exercise 4.51
        ((perm-assignment? exp) (analyze-perm-assignment exp))
        ;;exercise 4.52
        ((if-fail? exp) (analyze-if-fail exp))
        ;;exercise 4.54
        ((require? exp) (analyze-require exp))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

#|
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))
|#

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;;success continuation for evaluating the predicate
             ;;to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;;failure continuation for evaluating the predicate
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;;success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;;failure contiunation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)                ;*1*
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()            ;*2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;;success continuation for the aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;;success continuation for recursive
                                ;;call to get-args
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;;ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

;; Exercise 4.50
(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (delete-item list n)
  (define (loop first-item rest-items count)
    (cond ((= count 0) rest-items)
          ((null? rest-items)
           (error "List too small -- DELETE-ITEM"))
          (else
           (cons first-item
                 (loop (car rest-items) (cdr rest-items) (- count 1))))))
  (if (null? list)
      list
      (loop (car list) (cdr list) n)))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((n (random (length choices))))
              ((list-ref choices n) env
                                    succeed
                                    (lambda ()
                                      (try-next (delete-item choices n)))))))
      (try-next cprocs))))

;; Exercise 4.51
(define (perm-assignment? exp)
  (tagged-list? exp 'permanent-set!))
(define (perm-assignment-variable exp) (cadr exp))
(define (perm-assignment-value exp) (caddr exp))
(define (analyze-perm-assignment exp)
  (let ((var (perm-assignment-variable exp))
        (vproc (analyze (perm-assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))
#|
Using set! instead yields the values (a b 1) and (a c 1)
|#

;; Exercise 4.52
(define (if-fail? exp)
  (tagged-list? exp 'if-fail))
(define (if-fail-try exp) (cadr exp))
(define (if-fail-alt exp) (caddr exp))
(define (analyze-if-fail exp)
  (let ((tproc (analyze (if-fail-try exp)))
        (aproc (analyze (if-fail-alt exp))))
    (lambda (env succeed fail)
      (tproc env
             succeed
             (lambda ()
               (aproc env succeed fail))))))
(define the-global-environment (setup-environment))

;; Exercise 4.53
#|
((8 35) (3 110) (20 3))
|#

;; Exercise 4.54
(define (require? exp) (tagged-list? exp 'require))
(define (require-predicate exp) (cadr exp))
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (false? pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))