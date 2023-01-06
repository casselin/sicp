#lang sicp
(#%require "queue.rkt")
;;; Section 3.3.4 A Simulator for Digital Circuits
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; Logical operations
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (cond ((not (or (= s1 0) (= s1 1)))
         (error "Invalid signal" s1))
        ((not (or (= s2 0) (= s2 1)))
         (error "Invalid signal" s2))
        ((and (= s1 1) (= s2 1))
         1)
        (else 0)))

(define (logical-or s1 s2)
  (cond ((not (or (= s1 0) (= s1 1)))
         (error "Invalid signal" s1))
        ((not (or (= s2 0) (= s2 1)))
         (error "Invalid signal" s2))
        ((or (= s1 1) (= s2 1))
         1)
        (else 0)))

;; Primitive function boxes
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; Exercise 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;; Exercise 3.29
(define (or-gate2 a1 a2 output)
  (let ((b1 (make-wire)) (b2 (make-wire))
                         (and-out (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 and-out)
    (inverter and-out output)
    'ok))
#|
The implied or-gate-delay using this procedure will be
2*inverter-delay + and-gate-delay since the initial inverters' delay
will occur simultaneously
|#

;; Exercise 3.30
(define (ripple-carry-adder As Bs C Ss)
  (define (loop Ak Bk c-in Sk)
    (if (null? Ak)
        (begin
          (set-signal! c-in 0)
          'ok)
        (let ((c-out (make-wire)))
          (full-adder (car Ak) (car Bk) c-in (car Sk) c-out)
          (loop (cdr Ak) (cdr Bk) c-out (cdr Sk)))))
  (if (= (length As) (length Bs) (length Ss))
      (loop As Bs C Ss)
      (error "Wire lists not equal length -- RIPPLE-CARRY-ADDER"
             (list As Bs Ss))))
#|
The total delay time for an n-bit ripple-carry adder is
n * full-adder-delay
where
full-adder-delay = 2 * half-adder-delay + or-gate-delay
and
half-adder-delay = max(or-gate-delay, and-gate-delay + inverter-delay)
                 + and-gate-delay

Therefore the total delay time will be
2n*max(or-gate-delay, and-gate-delay + inverter-delay)
+ 2n*and-gate-delay + n*or-gate-delay
|#

; Representing wires
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; The agenda
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

;; Exercise 3.31
#|
The initialization is necessary to ensure the wires are in a valid state
once they are connected to a function box. In this particular example,
there is an inverter function box inside the half-adder. Without
initializing the inverter action, the input and output wires of the
inverter will both be set to 0, which is an invalid state.
|#

; Time segments
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

; Agendas
(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

; Global agenda
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

;; Exercise 3.32
#|
Maintaining a FIFO-ordering on the actions to perform at each time segment
ensures that the final state at that time segment will be correct, as it
maintains the correct intermediate states of each wire as it performs the
actions for that segment. This matters because when a wire adds an action
to the agenda, it provides a snapshot of its intermediate state. This means
that the order of each change is important. Thus, using a LIFO-ordering
instead will not respect the intermediate states correctly.

For example, using a FIFO-ordering:
(set-signal! input-1 1)
will place the action
(set-signal! output (logical-and 1 1)) in the agenda.
Next,
(set-signal! input-2 0)
will place the action
(set-signal! output (logical-and 1 0)) in the agenda.
Note that the correct signal for input-1 (1) is observed in this action.
Resolving this agenda and-gate-delay segments later in a FIFO-order:
(set-signal! output 1)
(set-signal! output 0)
and we arrive at the correct final state for this segment.

Contrast this with a LIFO-ordering:
(set-signal! input-1 1)
will place the action
(set-signal! output (logical-and 1 1)) in the agenda.
Next,
(set-signal! input-2 0)
will place the action
(set-signal! output (logical-and 1 0)) in the agenda.
Resolving this agenda and-gate-delay segments later in a LIFO-order:
(set-signal! output 0)
(set-signal! output 1)
arrives at a final state where the output signal is 1 when it should
be 0, an invalid final state.
|#