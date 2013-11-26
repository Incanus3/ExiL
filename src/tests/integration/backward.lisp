(in-package :integration-tests)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(defclass backward-integration-tests (test-case)
  ((env :reader env :initform exil::*current-environment*)))

(defmethod set-up ((tests backward-integration-tests))
  (complete-reset)
  (unwatch all))

(def-test-method test-fact-matching ((tests backward-integration-tests) :run nil)
  (deffacts world
    (in box hall)
    (color box blue)
    (size box big))

  (reset)

  (defgoal (in ?object hall))
  (defgoal (color ?object blue))
  (defgoal (size ?object big))

  (back-run)

  (with-slots (env) tests
    (assert-false (eenv::goals env))
    (assert-equal (eenv::used-substitutions env) '((?object . box)))))

(def-test-method test-fact-matching-with-backtracking
    ((tests backward-integration-tests) :run nil)
  (deffacts world
    (in box1 hall)
    (color box1 green)

    (in box2 hall)
    (color box2 blue)
    (size box2 small)

    (in box3 hall)
    (color box3 blue)
    (size box3 big))

  (reset)

  (defgoal (in ?object hall))
  (defgoal (color ?object blue))
  (defgoal (size ?object big))

  (back-run)

  (with-slots (env) tests
    (assert-false (eenv::goals env))
    (assert-equal (eenv::used-substitutions env) '((?object . box3)))))

(def-test-method test-fact-matching-with-alternative-answers
    ((tests backward-integration-tests) :run nil)
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

  (reset)

  (defgoal (in ?object hall))
  (defgoal (color ?object red))
  (defgoal (size ?object big))

  (back-run)

  (with-slots (env) tests
    (assert-false (eenv::goals env))
    (assert-equal (eenv::used-substitutions env) '((?object . box2)))

    (back-run)

    (assert-false (eenv::goals env))
    (assert-equal (eenv::used-substitutions env) '((?object . box4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test-method test-rule-matching
    ((tests backward-integration-tests) :run nil)
  (deffacts world
    (female jane)
    (parent-of jane george))

  (defrule mother
    (female ?mother)
    (parent-of ?mother ?child)
    =>
    (assert (mother-of ?mother ?child)))

  (reset)

  (defgoal (mother-of ?mother-of-george george))

  (back-run)

  (with-slots (env) tests
    (assert-false (eenv::goals env))
    (assert-equal (eenv::used-substitutions env) '((?mother-of-george . jane)))))

(def-test-method test-rule-matching-with-backtracking
    ((tests backward-integration-tests) :run nil)
  (deffacts world
    (female jane)
    (parent-of jane george))

  (defrule mother-is-daughter-of-grandpa
    (grandpa-of ?grandpa ?child)
    (daughter-of ?mother ?grandpa)
    =>
    (assert (mother-of ?mother ?child)))

  (defrule mother-is-female-parent
    (female ?mother)
    (parent-of ?mother ?child)
    =>
    (assert (mother-of ?mother ?child)))

  (reset)

  (defgoal (mother-of ?mother-of-george george))

  (back-run)

  (with-slots (env) tests
    (assert-false (eenv::goals env))
    (assert-equal (eenv::used-substitutions env) '((?mother-of-george . jane)))))

(def-test-method test-rule-matching-with-backtracking-inverted-rules
    ((tests backward-integration-tests) :run nil)
  (deffacts world
    (female jane)
    (parent-of jane george))

  (defrule mother-is-female-parent
    (female ?mother)
    (parent-of ?mother ?child)
    =>
    (assert (mother-of ?mother ?child)))

  (defrule mother-is-daughter-of-grandpa
    (grandpa-of ?grandpa ?child)
    (daughter-of ?mother ?grandpa)
    =>
    (assert (mother-of ?mother ?child)))

  (reset)

  (defgoal (mother-of ?mother-of-george george))

  (back-run)

  (with-slots (env) tests
    (assert-false (eenv::goals env))
    (assert-equal (eenv::used-substitutions env) '((?mother-of-george . jane)))))

(def-test-method test-rule-matching-with-alternative-answers
    ((tests backward-integration-tests) :run nil)
  (deffacts world
    (grandpa-of joseph george)
    (daughter-of jane joseph)
    (female jane)
    (parent-of jane george))

  (defrule mother-is-daughter-of-grandpa
    (grandpa-of ?grandpa ?child)
    (daughter-of ?mother ?grandpa)
    =>
    (assert (mother-of ?mother ?child)))

  (defrule mother-is-female-parent
    (female ?mother)
    (parent-of ?mother ?child)
    =>
    (assert (mother-of ?mother ?child)))

  (reset)

  (defgoal (mother-of ?mother-of-george george))

  (with-slots (env) tests
    (back-run)

    (assert-false (eenv::goals env))
    (assert-equal (eenv::all-used-substitutions env)
                  '((?mother-of-george . jane)
                    (?child . george)
                    (?grandpa . joseph)))

    (back-run)

    (assert-false (eenv::goals env))
    (assert-equal (eenv::all-used-substitutions env)
                  '((?mother-of-george . jane)
                    (?child . george)))))

(def-test-method test-backward-chaining-overall
    ((tests backward-integration-tests) :run nil)
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
  (defgoal (size ?object big))

  (back-run)

  (with-slots (env) tests
    (assert-false (eenv::goals env))
    (assert-equal (eenv::used-substitutions env) '((?object . box2)))

    (back-run)

    (assert-false (eenv::goals env))
    (assert-equal (eenv::used-substitutions env) '((?object . box3)))

    (back-run)

    (assert-false (eenv::goals env))
    (assert-equal (eenv::used-substitutions env) '((?object . box4)))))

(add-test-suite 'backward-integration-tests)
;;(textui-test-run (get-suite backward-integration-tests))
