(in-package :integration-tests)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(defclass backward-integration-tests (test-case)
  ((env :reader env :initform exil::*current-environment*)))

(defmethod set-up ((tests template-integration-tests))
  (complete-reset)
  (unwatch all))

(def-test-method test-fact-matching ((tests template-integration-tests) :run t)
  (deffacts world
    (in box hall)
    (color box blue)
    (size box big))

  (defgoal (in ?object hall))
  (defgoal (color ?object blue))
  (defgoal (size ?object big))

  (reset)
  (back-run)

  ;; ASSERT USED BINDINGS HERE, IDEALLY USING ONLY FRONT-END API
  ;; (with-slots (env) tests
  ;;   (let ((in-template (eenv::find-template env :in)))
  ;;     (assert-true (eenv::find-fact env (eenv::make-template-fact
  ;;       				 in-template '(:object box :location A))))))
  )

(def-test-method test-fact-matching-with-backtracking
    ((tests template-integration-tests) :run t)
  (deffacts world
    (in box1 hall)
    (color box1 green)

    (in box2 hall)
    (color box2 blue)
    (size box2 small)

    (in box3 hall)
    (color box3 blue)
    (size box3 big))

  (defgoal (in ?object hall))
  (defgoal (color ?object blue))
  (defgoal (size ?object big))

  (reset)
  (back-run)

  ;; ASSERT USED BINDINGS HERE
  ;; (with-slots (env) tests
  ;;   (let ((in-template (eenv::find-template env :in)))
  ;;     (assert-true (eenv::find-fact env (eenv::make-template-fact
  ;;       				 in-template '(:object box :location A))))))
  )

(add-test-suite 'template-integration-tests)
                                        ;(textui-test-run (get-suite template-integration-tests))
