(in-package :exil-user)

(princ "========================================================================")

(complete-reset)
(unwatch all)
(watch facts)

(deffacts world
  (have-money))

(defrule buy-car
  (have-money)
  =>
  (retract (have-money))
  (assert (have-car)))

(defrule pay-rent
  (have-money)
  =>
  (retract (have-money))
  (assert (rent-payed)))

(reset)
(defgoal (rent-payed))

(format t "~%~%BACKWARD:")
(defgoal (rent-payed))
(back-run)

(format t "~%~%FORWARD:")
(run)
