(in-package :exil-user)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(setf erete::*debug-rete* nil)

(format t "~%~%Running examples with template facts:~%")

(complete-reset)

(deftemplate goal action object from to)
(deftemplate in object location)

(deffacts world
  (in :object robot :location A)
  (in :object box :location B)
  (goal :action push :object box :from B :to A))

(defrule move
  (goal :object ?x :from ?y)
  (in :object ?x :location ?y)
  (- in :object robot :location ?y)
  ?robot <- (in :object robot :location ?)
  =>
  (modify ?robot :location ?y))

(defrule push
  (goal :object ?x :from ?y :to ?z)
  ?object <- (in :object ?x :location ?y)
  ?robot <- (in :object robot :location ?y)
  =>
  (modify ?robot :location ?z)
  (modify ?object :location ?z))

(defrule stop
  (goal :object ?x :to ?y)
  (in :object ?x :location ?y)
  =>
  (halt))

(unwatch all)
(watch facts)
;(watch activations)

(reset)

#|
(step)
|#

;#|
(run)
;|#
