(in-package :exil-user)

(complete-reset)

(undo) ; should do nothing
(watch facts)
(watch activations)

(undo)
(undo)
(redo)
(redo)

(deftemplate in object location)
(deftemplate in blah foo bar)
(undo)

(deftemplate goal object location)
(redo) ; should do nothing

(defrule move
  (goal :object ?object :location ?goalloc)
  (in :object ?object :location ?objloc)
  (- in :object robot :location ?objloc)
  ?robot-position <- (in :object robot :location ?)
  =>
  (modify ?robot-position :location ?objloc))
(undo)
(redo)

(defrule push
  (goal :object ?object :location ?goalloc)
  (- in :object ?object :location ?goalloc)
  ?object-position <- (in :object ?object :location ?objloc)
  ?robot-position <- (in :object robot :location ?objloc)
  =>
  (modify ?object-position :location ?goalloc)
  (modify ?robot-position :location ?goalloc))

(defrule stop
  (goal :object ?object :location ?goalloc)
  (in :object ?object :location ?goalloc)
  =>
  (halt))

(deffacts world
	     (goal :object box :location b)
	     (in :object box :location a)
	     (in :object robot :location b))
(undo)
(redo)

(reset)
(undo)
(redo)

(step)
(step)
(undo)
(undo)
(redo)
(redo)
(step)

(reset)
(run)
(undo)
(redo)

#|
(facts)
(agenda)
|#
