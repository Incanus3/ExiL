(in-package :integration-tests)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(defclass undo-redo-backward-tests (test-case)
  ((env :reader env :initform exil::*current-environment*)))

(defmethod set-up ((tests undo-redo-backward-tests))
  (complete-reset)
  (unwatch all)

  (deffacts world
    (in box1 hall)
    (color box1 blue)

    (in box2 hall)
    (color box2 red)
    (size box2 big)

    (in box3 hall)
    (color box3 red)
    (size box3 small)

    (in box4 hall)
    (color box4 red)
    (size box4 big))

  ;; this actually adds conditions to goals again, but since only
  ;; assertions are allowed in backward-chaining rules' activations
  ;; this will never cycle
  (defrule grow-box
    (in ?box hall)
    (color ?box red)
    (size ?box small)
    =>
    (assert (size ?box big)))

  (reset)

  (defgoal (in ?object hall))
  (defgoal (color ?object red))
  (defgoal (size ?object big)))

(def-test-method undo-back-step-test ((tests undo-redo-backward-tests) :run nil)
  (with-slots (env) tests
    (back-step)

    (assert-true (eenv::find-goal
                  env (eenv::make-simple-pattern '(color box1 red))))

    (undo)

    (assert-true (eenv::find-goal
                  env (eenv::make-simple-pattern '(color ?object red))))

    (redo)

    (assert-true (eenv::find-goal
                  env (eenv::make-simple-pattern '(color box1 red))))))

(def-test-method undo-back-step-test ((tests undo-redo-backward-tests) :run nil)
  (with-slots (env) tests
    (back-run)

    (assert-false (goals))

    (undo)

    (assert-true (eenv::find-goal
                  env (eenv::make-simple-pattern '(color ?object red))))

    (redo)

    (assert-false (goals))))

(add-test-suite 'undo-redo-backward-tests)
;(textui-test-run (get-suite undo-redo-backward-tests))
