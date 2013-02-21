(in-package :exil-user)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(format t "~%~%Running examples with simple facts:~%")

; SIMPLE FACTS:
;(clear)
(complete-reset)

(deffacts world
  (in robot A)
  (in box B)
  (goal push box B A))

(defrule move
  (goal ?action ?object ?from ?to)
  (in ?object ?from)
  (- in robot ?from)
  (in robot ?z)
  =>
  (retract (in robot ?z))
  (assert (in robot ?from)))

(defrule push
  (goal ?action ?object ?from ?to)
  (in ?object ?from)
  (in robot ?from)
  =>
  (retract (in robot ?from))
  (assert (in robot ?to))
  (retract (in ?object ?from))
  (assert (in ?object ?to)))

(defrule stop
  (goal ?action ?object ?from ?to)
  (in ?object ?to)
  =>
  (halt))

(unwatch all)
(watch facts)
;(watch activations)

(reset)

;(step)
(run)
