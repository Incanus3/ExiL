(in-package :integration-tests)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(defclass undo-redo-integration-tests (integration-tests) ())

(defmethod set-up ((tests undo-redo-integration-tests))
  (call-next-method)

  (undo)				; should do nothing
  (watch facts)
  (watch activations)

  (undo)
  (undo)

  (deftemplate in object location)
  (deftemplate in blah foo bar)
  (undo)

  (deftemplate goal object location)
  (redo)				; should do nothing

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
  (redo))

(def-test-method undo-redo-integration-test ((tests undo-redo-integration-tests) :run nil)
  (with-slots (env) tests
    (let ((in-template (eenv::find-template env :in)))
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

      (assert-true (eenv::find-fact
        	    env (eenv::make-template-fact
        		 in-template '(:object box :location b))))

      (reset)
      (run)
      (undo)
      (redo)

      (assert-true (eenv::find-fact
        	    env (eenv::make-template-fact
        		 in-template '(:object box :location b)))))))

(add-test-suite 'undo-redo-integration-tests)
;(textui-test-run (get-suite undo-redo-integration-tests))
