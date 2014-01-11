(in-package :integration-tests)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(defclass rete-bugs-int-tests (integration-tests) ())

(def-test-method tests-for-prev-neg-conds ((tests rete-bugs-int-tests) :run t)
  (deffacts world
    (in obj1 a)
    (in obj2 b))

  (defrule rule
    (in obj1 ?loc1)
    (- next-to ?loc1 ?loc2)
    (in obj2 ?loc2)
    =>)

  (reset)

  (assert-true (agenda)))

(add-test-suite 'rete-bugs-int-tests)
;;(textui-test-run (get-suite rete-bugs-int-tests))
