(in-package :integration-tests)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(defclass clips-integration-tests (integration-tests) ())

(defmethod set-up ((tests clips-integration-tests))
  (call-next-method)

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
    (goal (action push) (object ?x) (to ?y))
    (in (object ?x) (location ?y))
    =>
    (halt))

  (defrule move
    (goal (action push) (object ?x) (from ?y))
    (in (object ?x) (location ?y))
    (- in (object robot) (location ?y))
    ?robot <- (in (object robot) (location ?z))
    =>
    (modify ?robot (location ?y)))

  (defrule push
    (goal (action push) (object ?x) (from ?y) (to ?z))
    ?object <- (in (object ?x) (location ?y))
    ?robot <- (in (object robot) (location ?y))
    =>
    (modify ?robot (location ?z))
    (modify ?object (location ?z)))

  (unwatch all))

(def-test-method clips-integration-test ((tests clips-integration-tests) :run nil)
  (reset)
  (run)
  (with-slots (env) tests
    (let ((in-template (eenv::find-template env :in)))
      (assert-true (eenv::find-fact env (eenv::make-template-fact
					 in-template '(:object box :location A)))))))

(add-test-suite 'clips-integration-tests)
;(textui-test-run (get-suite clips-integration-tests))
