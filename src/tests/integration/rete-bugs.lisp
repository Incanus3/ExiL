(in-package :integration-tests)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(defclass rete-bugs-int-tests (integration-tests) ())

(def-test-method tests-for-prev-neg-conds ((tests rete-bugs-int-tests) :run nil)
  (deffacts world
    (in obj1 a)
    (in obj2 b))

  ;; this should now match all pairs of facts (in obj1 ?) (in obj2 ?)
  (defrule rule
    (in obj1 ?loc1)
    (- next-to ?loc1 ?loc2)
    (in obj2 ?loc2)
    =>)

  (reset)
  ;; agenda prints to output and returns nil
  (assert-true (exil-env:agenda (env tests))))

(add-test-suite 'rete-bugs-int-tests)
;;(textui-test-run (get-suite rete-bugs-int-tests))
