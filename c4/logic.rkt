#lang sicp
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