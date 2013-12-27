(in-package :exil-user)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(format t "~%~%Running examples with simple facts:~%")

(complete-reset)

(deffacts world
  (in robot A)
  (in box B)
  (goal move box B A))

(defrule move-robot
  (goal move ?object ?from ?to)
  (in ?object ?from)
  (- in robot ?from)
  (in robot ?z)
  =>
  (retract (in robot ?z))
  (assert (in robot ?from)))

(defrule move-object
  (goal move ?object ?from ?to)
  (in ?object ?from)
  (in robot ?from)
  =>
  (retract (in robot ?from))
  (assert (in robot ?to))
  (retract (in ?object ?from))
  (assert (in ?object ?to)))

(defrule stop
  ?goal <- (goal move ?object ?from ?to)
  (in ?object ?to)
  =>
  (retract ?goal)
  (halt))

(unwatch all)
(watch facts)
;(watch activations)

(reset)

(run)

#|
(step)
|#

(complete-reset)
