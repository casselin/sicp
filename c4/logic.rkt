#lang sicp
(#%require "table.rkt")
(#%require "stream.rkt")
;;; Logic Programming

;; Exercise 4.55
#|
a.
(supervisor ?name (Bitdiddle Ben))
b.
(job ?name (accounting . ?job))
c.
(address ?name (Slumerville . ?address))
|#

;; Exercise 4.56
#|
a.
(and (supervisor ?name (Bitdiddle Ben))
     (address ?name ?address))
b.
(and (salary (Bitdiddle Ben) ?bensalary)
     (salary ?name ?salary)
     (lisp-value < ?salary ?bensalary))
c.
(and (supervisor ?employee ?manager)
     (job ?manager ?job)
     (not (job ?manager (computer . ?title))))
|#

;; Exercise 4.57
#|
(rule (can-replace ?person-1 person-2)
      (and (not (same ?person-1 ?person-2))
           (or (and (job ?person-1 ?job-1)
                    (job ?person-2 ?job-1))
               (can-do-job ?job-1 ?job-2))))
a.
(can-replace ?x (Fect Cy D)
b.
(and (can-replace ?replacer ?replacee)
     (salary ?replacer ?replacer-sal)
     (salary ?replacee ?replacee-sal)
     (list-value < ?replacer-sal ?replacee-sal))
|#

;; Exercise 4.58
#|
(rule (big-shot ?person ?division)
      (and (job ?person (?division . ?title))
           (or (not (supervisor ?person ?manager))
               (and (supervisor ?person ?manager)
                    (not (job ?manager (?division . ?manager-title)))))))
|#

;; Exercise 4.59
#|
a.
(meeting ?division (Friday ?time))
b.
(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and (job ?person (?division . ?title))
               (meeting ?division ?day-and-time))))
c.
(meeting-time (Hacker Alyssa P) (Wednesday ?time))
|#

;; Exercise 4.60
#|
This happens because each pair of people will belong to the sets ?person-1
and ?person-2. Thus when ?person-1 is instantiated with one of the pair
instantiating ?person-2 with the other will satisfy the lives-near rule,
and vice versa.
One way to fix this could be to impose an ordering on ?person-1 and ?person-2.
Thus only one of
(lives-near ?person-1 ?person-2) or (lives-near ?person-2 ?person-1)
is true. This would sacrifice some of the generality of the rule, as any
user would have to know about this ordering. To compensate for this, the
changed lives-near rule could be renamed to lives-near-ordered and the new
lives-near rule would be
(rule (lives-near ?person-1 ?person-2)
      (or (lives-near-ordered ?person-1 ?person-2)
          (lives-near-ordered ?person-2 ?person-1)))
|#

;; Exercise 4.61
#|
> (?x next-to ?y in (1 (2 3) 4))
(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))

> (?x next-to 1 in (2 1 3 1))
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))
|#

;; Exercise 4.62
#|
We will interpret the relation (last-pair x y) as "y is the last pair of x"
(rule (last-pair (?u . ()) (?u . ())))

(rule (last-pair (?v . ?w) ?y)
      (last-pair ?w ?y))
|#

;; Exercise 4.63
#|
(rule (grandson ?grandfather ?grandson)
      (and (child ?father ?grandson)
           (child ?grandfather ?father)))

(rule (child ?father ?son)
      (or (son ?father ?son)
          (and (wife ?father ?mother)
               (son ?mother ?son))))

> (grandson Cain ?x)
> (child Lamech ?x)
> (grandson Methushael ?x)
|#

;; Exercise 4.64
#|
The interpreter finds the rule outranked-by satisfies the pattern in the
query and thus binds ?staff-person to (Bitdiddle Ben) and ?boss to ?who.
Since the queries in the or clause are run in parallel and the recursive
query is the first query of the and clause, the interpreter will attempt
to evaluate (outranked-by ?middle-manager ?boss) where only ?boss is bound
to ?who and ?middle-manager is unbound. Thus the interpreter will attempt to
evaluate (outranked-by ?middle-manager ?who), which results in an infinite
loop.
The reason the other version works is because having the first query in the
and clause as (supervisor ?staff-person ?middle-manager) produces frames
(or fails) in which ?middle-manager will be bound to objects in
the data base.
|#

;; Exercise 4.65
#|
Oliver Warbucks is listed four times because there are four frames that
satisfy the body of the wheel rule. Once ?person is bound to
(Warbucks Oliver), there are two choices for ?middle-manager:
(Bitdiddle Ben) and (Scrooge Eben). For the next query in the and clause
of the wheel rule body, there are three choices that satisfy the query
when ?middle-manager is bound to (Bitdiddle Ben) (ie. the computer division),
and there is one choice that satisfies the query when ?middle-manager is
bound to (Scrooge Eben).
|#

;; Exercise 4.66
#|
Ben has just realized that the elements of the stream are not unique.
One way to salvage this would be to filter out repeat results. This can
be accomplished by performing a stream accumulation that, for each new
element of the stream, filters the remaining stream of all equal elements.
This will remove any duplicate entries, but is quadratic in the length of
the stream.
|#

;; Exercise 4.67
#|
To detect the simple loops described in the text and Exercise 4.64 it is
enough to detect when a query is the same as a previous rule application.
Thus the history will be a list of rule conclusions, instantiated with the
values from the extended frame after unification succeeds. Checking and
updating the history will be done by apply-a-rule (previously I thought
qeval would be best for this, but since rule applications are what will
cause these infinite loops many checks would be done pointlessly by qeval).
|#
(define (new-history) '())
(define (in-history? entry history)
  (member entry history))
(define (make-history-entry pattern frame)
  (instantiate pattern
               frame
               (lambda (v f)
                 (canonicalize-variable v))))
(define (extend-history entry history)
  (cons entry history))
(define (canonicalize-variable var)
  (string->symbol
   (string-append "?"
    (if (number? (cadr var))
        (symbol->string (caddr var))
        (symbol->string (cadr var))))))
(define (display-loop-notification pattern)
  (newline)
  (display "Loop detected on pattern: ")
  (display pattern))

;; Exercise 4.68
#|
This will not resolve (reverse ?x (3 2 1)):
(rule (reverse () ()))
(rule (reverse (?u . ?v) ?y)
  (and (reverse ?v ?w)
       (append-to-form ?w (?u . ()) ?y)))

The reverse rules will run into the same problem with infinite loops as the
married rule presented in the text. This is because solving
(reverse ?x (3 2 1)) will need to rely on a similar rule:
(rule (reverse ?x ?y) (reverse ?y ?x))
I believe that any attempt to assert this rule indirectly will always encounter
issues of infinite loops.
|#

;; Exercise 4.69
#|
(rule (?x ends-with ?y)
  (last-pair ?x ?y))
(rule ((grandson) ?x ?y) (grandson ?x ?y))
(rule ((great . ?rel) ?x ?y)
  (and (child ?x ?z)
       (?rel ?z ?y)))
       (?rel ends-with grandson)
|#
(define qeval-table (make-table))
(define get (qeval-table 'lookup-proc))
(define put (qeval-table 'insert-proc!))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

;;; Driver-Loop and Instantiation

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream '()) (new-history))))
           (query-driver-loop)))))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

;;; The Evaluator

(define (qeval query frame-stream history)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream history)
        (simple-query query frame-stream history))))

;; Simple queries

(define (simple-query query-pattern frame-stream history)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame history))))
   frame-stream))

;; Compound queries

(define (conjoin conjuncts frame-stream history)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream
                      history)
               history)))
(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream history)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream history)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream
                       history)))))
(put 'or 'qeval disjoin)

;; Filters

(define (negate operands frame-stream history)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)
                              history))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put 'not 'qeval negate)

(define (lisp-value call frame-stream history)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
            call
            frame
            (lambda (v f)
              (error "Unknown pat var -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) (interaction-environment))
         (args exp)))

(define (always-true ignore frame-stream history) frame-stream)
(put 'always-true 'qeval always-true)

;;; Pattern Matcher

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

;; Rules and Unification

(define (apply-rules pattern frame history)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame history))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame history)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (let ((new-event
                 (make-history-entry (conclusion clean-rule)
                                     unify-result)))
            (if (in-history? new-event history)
                (begin
                  (display-loop-notification new-event)
                  the-empty-stream)
                (qeval (rule-body clean-rule)
                       (singleton-stream unify-result)
                       (extend-history new-event history))))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2))
         (unify-match (cdr p1)
                      (cdr p2)
                      (unify-match (car p1)
                                   (car p2)
                                   frame)))
        (else 'failed)))

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  var (binding-value binding) frame)
                 (extend var val frame))))
          ((depends-on? val var frame)
           'failed)
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))

;;; Data Base

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))
(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))
(define (use-index? pat)
  (constant-symbol? (car pat)))

;; Exercise 4.70
#|
The purpose of the let binding in add-assertion! and add-rule! is to create
a new environment where the previous stream is bound before the call to set!
is made. This must be done to preserve the previous copy of the stream.
Since the second argument to cons-stream is delayed, THE-ASSERTIONS will
not be evaluated in the call
(set! THE-ASSERTIONS (cons-stream assertion THE-ASSERTIONS)). This means that
when the cdr of this stream is eventually forced, it will now be referring to
the new version of THE-ASSERTIONS which will result in an infinite stream of
the assertion passed to add-assertion!.
|#

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))
(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))
(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

(define (negated-query exps) (car exps))

(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (rule? statement)
  (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))
(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))
(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp)
  (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
    (if (number? (cadr variable))
        (string-append (symbol->string (caddr variable))
                       "-"
                       (number->string (cadr variable)))
        (symbol->string (cadr variable))))))

(define (make-binding variable value)
  (cons variable value))
(define (binding-variable binding)
  (car binding))
(define (binding-value binding)
  (cdr binding))
(define (binding-in-frame variable frame)
  (assoc variable frame))
(define (extend variable value frame)
  (cons (make-binding variable value) frame))