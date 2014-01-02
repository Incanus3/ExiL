(in-package :exil-user)

(princ "========================================================================")

(complete-reset)
(unwatch all)
(watch facts)


(deffacts boxes
  (space on 1)
  (space on 2)
  (space on 3)
  (1 on table)
  (2 on table)
  (3 on table)
  ;; when we switch the goals, forward chaining makes the wrong move
  (goal 1 on 2)
  (goal 2 on 3))

(defrule move-box
  (goal ?a on ?c)
  (space on ?a)
  (?a on ?b)
  (space on ?c)
  =>
  (retract (?a on ?b))
  (unless (equalp '?c 'table)
    (retract (space on ?c)))
  (assert (?a on ?c))
  (unless (equalp '?b 'table)
    (assert (space on ?b))))


(reset)

;;(step)
(run)
