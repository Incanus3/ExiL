(in-package :exil-user)

;; Based on example 'Monkey and Bananas' from Artificial Intelligence
;; programming: Case studies in Common Lisp (Peter Norvig)

(princ "========================================================================")

(complete-reset)
(unwatch all)
(watch facts)


(deffacts monkey
  (at monkey garden)
  (at chair table)
  (at bananas tree)
  (on floor)
  (has ball)
  (hungry))

(defrule climb-on-chair
  (at bananas ?bpos)
  (at chair ?bpos)
  (at monkey ?bpos)
  (on floor)
  =>
  (retract (on floor))
  (assert (on chair)))

(defrule go-to-chair
  (at chair ?cpos)
  (- at monkey ?cpos)
  ?mpos <- (at monkey ?)
  =>
  (retract ?mpos)
  (assert (at monkey ?cpos)))

(defrule push-chair-to-bananas
  (at bananas ?bpos)
  (- at chair ?bpos)
  (at chair ?cpos)
  (at monkey ?cpos)
  =>
  (retract (at chair ?cpos))
  (retract (at monkey ?cpos))
  (assert (at chair ?bpos))
  (assert (at monkey ?bpos)))

(defrule grasp-bananas
  (at bananas ?bpos)
  (at monkey ?bpos)
  (on chair)
  (empty-handed)
  =>
  (retract (empty-handed))
  (retract (at bananas ?bpos))
  (assert (has bananas)))

(defrule drop-ball
  (has ball)
  =>
  (retract (has ball))
  (assert (empty-handed)))

(defrule eat-bananas
  (has bananas)
  =>
  (retract (has bananas))
  (retract (hungry))
  (assert (empty-handed))
  (assert (not hungry)))

(reset)

(defgoal (not hungry))
;; this loops in second step
;; (back-step)

;; (step)
(run)
