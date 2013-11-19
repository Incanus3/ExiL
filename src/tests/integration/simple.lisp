(in-package :integration-tests)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(defclass simple-integration-tests (test-case)
  ((env :reader env :initform exil::*current-environment*)))

(defmethod set-up ((tests simple-integration-tests))
  (complete-reset)

  (deffacts world
    (in robot A)
    (in box B)
    (goal push box B A))

  (defrule move
    (goal push ?object ?from ?to)
    (in ?object ?from)
    (- in robot ?from)
    (in robot ?z)
    =>
    (retract (in robot ?z))
    (assert (in robot ?from)))

  (defrule push
    (goal push ?object ?from ?to)
    (in ?object ?from)
    (in robot ?from)
    =>
    (retract (in robot ?from))
    (assert (in robot ?to))
    (retract (in ?object ?from))
    (assert (in ?object ?to)))

  (defrule stop
    (goal push ?object ?from ?to)
    (in ?object ?to)
    =>
    (halt))

  (unwatch all))

(def-test-method simple-integration-test ((tests simple-integration-tests) :run nil)
  (reset)
  (run)
  (assert-true (eenv::find-fact (env tests) (eenv::make-simple-fact '(in box A)))))

(add-test-suite 'simple-integration-tests)
;(textui-test-run (get-suite simple-integration-tests))
