(in-package :env-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass backward-env-tests (test-case)
  ((env :accessor env :initform (make-environment))))

(def-test-method test-add-goal ((tests backward-env-tests) :run nil)
  (with-slots (env) tests
    (let ((goal (make-simple-pattern '(is-child-of ?child john))))
      (assert-false (find-goal env goal))
      (add-goal env goal)
      (assert-true (find-goal env goal)))))

(add-test-suite 'backward-env-tests)
;(textui-test-run (get-suite backward-env-tests))
