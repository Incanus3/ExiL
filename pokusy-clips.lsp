(in-package :exil-user)

; TEMPLATE FACTS:
(clear)

(deftemplate goal
  (slot action)
  (slot object)
  (slot from)
  (slot to))

(deftemplate in
  (slot object)
  (slot location))

(deffacts world
  (in (object robot) (location A))
  (in (object box) (location B))
  (goal (action push) (object box) (from B) (to A)))

(defrule stop
  (goal (object ?x) (to ?y))
  (in (object ?x) (location ?y))
  =>
  (halt))

(defrule move
  (goal (object ?x) (from ?y))
  (in (object ?x) (location ?y))
  (- in (object robot) (location ?y))
  ?robot <- (in (object robot) (location ?z))
  =>
  (modify ?robot (location ?y)))

(defrule push
  (goal (object ?x) (from ?y) (to ?z))
  ?object <- (in (object ?x) (location ?y))
  ?robot <- (in (object robot) (location ?y))
  =>
  (modify ?robot (location ?z))
  (modify ?object (location ?z)))

(unwatch all)
(watch facts)
;(watch activations)

(reset)
;(step)

(run)