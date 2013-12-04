(in-package :integration-tests)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(defclass template-integration-tests (integration-tests) ())

(defmethod set-up ((tests template-integration-tests))
  (call-next-method)

  (deftemplate goal action object from to)
  (deftemplate in object location)

  (deffacts world
    (in :object robot :location A)
    (in :object box :location B)
    (goal :action push :object box :from B :to A))

  (defrule move
    (goal :action push :object ?x :from ?y)
    (in :object ?x :location ?y)
    (- in :object robot :location ?y)
    ?robot <- (in :object robot :location ?)
    =>
    (modify ?robot :location ?y))

  (defrule push
    (goal :action push :object ?x :from ?y :to ?z)
    ?object <- (in :object ?x :location ?y)
    ?robot <- (in :object robot :location ?y)
    =>
    (modify ?robot :location ?z)
    (modify ?object :location ?z))

  (defrule stop
    (goal :action push :object ?x :to ?y)
    (in :object ?x :location ?y)
    =>
    (halt))

  (unwatch all))

(def-test-method template-integration-test ((tests template-integration-tests) :run nil)
  (reset)
  (run)
  (with-slots (env) tests
    (let ((in-template (eenv::find-template env :in)))
      (assert-true (eenv::find-fact env (eenv::make-template-fact
					 in-template '(:object box :location A)))))))

(add-test-suite 'template-integration-tests)
;(textui-test-run (get-suite template-integration-tests))
