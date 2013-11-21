(in-package :env-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

;; rules' RHS is evaluated by eval, which doesn't have acces to the local
;; environment at the time of rule creation
(defvar *env*)

(defclass simple-env-tests (test-case)
  ((env :accessor env :initform (make-environment))))

(defgeneric set-up-move (tests))
(defmethod set-up-move ((tests simple-env-tests))
  (with-slots (env) tests
    (let ((rule (make-rule
		 :move
		 (list (make-simple-pattern '(goal ?action ?object ?from ?to))
		       (make-simple-pattern '(in ?object ?from))
		       (make-simple-pattern '(in robot ?from) :negated t)
		       (make-simple-pattern '(in robot ?z)))
		 (list '(rem-fact *env* (make-simple-fact '(in robot ?z)))
		       '(add-fact *env* (make-simple-fact '(in robot ?from)))))))
      (add-rule env rule))))

(defgeneric set-up-push (tests))
(defmethod set-up-push ((tests simple-env-tests))
  (with-slots (env) tests
    (let ((rule (make-rule
		 :push
		 (list (make-simple-pattern '(goal ?action ?object ?from ?to))
		       (make-simple-pattern '(in ?object ?from))
		       (make-simple-pattern '(in robot ?from)))
		 (list '(rem-fact *env* (make-simple-fact '(in robot ?from)))
		       '(add-fact *env* (make-simple-fact '(in robot ?to)))
		       '(rem-fact *env* (make-simple-fact '(in ?object ?from)))
		       '(add-fact *env* (make-simple-fact '(in ?object ?to)))))))
      (add-rule env rule))))

(defgeneric set-up-stop (tests))
(defmethod set-up-stop ((tests simple-env-tests))
  (with-slots (env) tests
    (let ((rule (make-rule
		 :stop
		 (list (make-simple-pattern '(goal ?action ?object ?from ?to))
		       (make-simple-pattern '(in ?object ?to)))
		 ())))
      (add-rule env rule))))

(defgeneric add-facts (tests))
(defmethod add-facts ((tests simple-env-tests))
  (with-slots (env) tests
    (add-fact env (make-simple-fact '(in robot A)))
    (add-fact env (make-simple-fact '(in box B)))
    (add-fact env (make-simple-fact '(goal push box B A)))))

(defmethod set-up ((tests simple-env-tests))
  (setf *env* (env tests))
  (set-up-move tests)
  (set-up-push tests)
  (set-up-stop tests)
  (add-facts tests))

(def-test-method test-simple-env ((tests simple-env-tests) :run nil)
  (with-slots (env) tests
    (step-env env)
    (step-env env)
    (step-env env)
    (assert-true (find-fact env (make-simple-fact '(in box A))))))

(add-test-suite 'simple-env-tests)
;(textui-test-run (get-suite simple-env-tests))
