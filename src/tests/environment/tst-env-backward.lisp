(in-package :env-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass backward-env-tests (test-case)
  ((env :accessor env)))

(defmethod set-up ((tests backward-env-tests))
  (setf (env tests) (make-environment)))

(def-test-method test-add-goal ((tests backward-env-tests) :run nil)
  (with-slots (env) tests
    (let ((goal (make-simple-pattern '(is-child-of ?child john))))
      (assert-false (find-goal env goal))
      (add-goal env goal)
      (assert-true (find-goal env goal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test-method test-fact-matching ((tests backward-env-tests) :run nil)
  (with-slots (env) tests
    (add-fact env (make-simple-fact '(in box hall)))
    (add-fact env (make-simple-fact '(color box blue)))
    (add-fact env (make-simple-fact '(size box big)))

    (add-goal env (make-simple-pattern '(in ?object hall)))
    (add-goal env (make-simple-pattern '(color ?object blue)))
    (add-goal env (make-simple-pattern '(size ?object big)))

    (let ((substitutions (back-run env)))
      (assert-false (eenv::goals env))
      (assert-equal substitutions '((?object . box))))))

(def-test-method test-fact-matching-with-backtracking
    ((tests backward-env-tests) :run nil)
  (with-slots (env) tests
    (add-fact env (make-simple-fact '(in box1 hall)))
    (add-fact env (make-simple-fact '(color box1 green)))
    (add-fact env (make-simple-fact '(in box2 hall)))
    (add-fact env (make-simple-fact '(color box2 blue)))
    (add-fact env (make-simple-fact '(size box2 small)))
    (add-fact env (make-simple-fact '(in box3 hall)))
    (add-fact env (make-simple-fact '(color box3 blue)))
    (add-fact env (make-simple-fact '(size box3 big)))

    (add-goal env (make-simple-pattern '(in ?object hall)))
    (add-goal env (make-simple-pattern '(color ?object blue)))
    (add-goal env (make-simple-pattern '(size ?object big)))

    (let ((substitutions (back-run env)))
      (assert-false (eenv::goals env))
      (assert-equal substitutions '((?object . box3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-test-method test-rule-matching ((tests backward-env-tests) :run nil)
  (with-slots (env) tests
    (add-fact env (make-simple-fact '(female jane)))
    (add-fact env (make-simple-fact '(parent-of jane george)))

    (add-rule env (make-rule
		   :mother
		   (list (make-simple-pattern '(female ?mother))
			 (make-simple-pattern '(parent-of ?mother ?child)))
		   (list '(assert (mother-of ?mother ?child)))))

    (add-goal env (make-simple-pattern '(mother-of ?mother-of-george george)))

    (let ((substitutions (back-run env)))
      (assert-false (eenv::goals env))
      (assert-equal substitutions '((?mother-of-george . jane))))))

(def-test-method test-rule-matching-with-backtracking
    ((tests backward-env-tests) :run nil)
  (with-slots (env) tests
    (add-fact env (make-simple-fact '(female jane)))
    (add-fact env (make-simple-fact '(parent-of jane george)))

    (add-rule env (make-rule
		   :mother-is-daughter-of-grandpa
		   (list (make-simple-pattern '(grandpa-of ?grandpa ?child))
			 (make-simple-pattern '(daughter-of ?mother ?grandpa)))
		   (list '(assert (mother-of ?mother ?child)))))
    (add-rule env (make-rule
		   :mother-is-female-parent
		   (list (make-simple-pattern '(female ?mother))
			 (make-simple-pattern '(parent-of ?mother ?child)))
		   (list '(assert (mother-of ?mother ?child)))))

    (add-goal env (make-simple-pattern '(mother-of ?mother-of-george george)))

    (let ((substitutions (back-run env)))
      (assert-false (eenv::goals env))
      (assert-equal substitutions '((?mother-of-george . jane))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-test-suite 'backward-env-tests)
;(textui-test-run (get-suite backward-env-tests))
