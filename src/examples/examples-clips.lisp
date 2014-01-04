(in-package :exil-user)

(format t "~%~%Running examples with clips-compatible syntax:~%")

(complete-reset)

(deftemplate goal
  (slot action (default move))
  (slot object)
  (slot from)
  (slot to))

(deftemplate in
  (slot object)
  (slot location))

(deffacts world
  (in (object robot) (location A))
  (in (object box) (location B))
  (goal (object box) (from B) (to A)))

(defrule move-robot
  (goal (action move) (object ?obj) (from ?from))
  (in (object ?obj) (location ?from))
  (- in (object robot) (location ?from))
  ?robot <- (in (object robot) (location ?z))
  =>
  (modify ?robot (location ?from)))

(defrule move-object
  (goal (action move) (object ?obj) (from ?from) (to ?to))
  ?object <- (in (object ?obj) (location ?from))
  ?robot <- (in (object robot) (location ?from))
  =>
  (modify ?robot (location ?to))
  (modify ?object (location ?to)))

(defrule stop
  ?goal <- (goal (action move) (object ?obj) (to ?to))
  (in (object ?obj) (location ?to))
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
