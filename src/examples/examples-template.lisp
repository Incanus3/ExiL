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
  (goal :action push :object ?obj :from ?from)
  (in :object ?obj :location ?from)
  (- in :object robot :location ?from)
  ?robot <- (in :object robot :location ?)
  =>
  (modify ?robot :location ?from))

(defrule push
  (goal :action push :object ?obj :from ?from :to ?to)
  ?object <- (in :object ?obj :location ?from)
  ?robot <- (in :object robot :location ?from)
  =>
  (modify ?robot :location ?to)
  (modify ?object :location ?to))

(defrule stop
  ?goal <- (goal :action push :object ?obj :to ?to)
  (in :object ?obj :location ?to)
  =>
  (retract ?goal)
  (halt))

(unwatch all)
;(watch facts)
;(watch rules)
(watch activations)

(reset)

#|
(step)
|#

(run)

;(complete-reset)
