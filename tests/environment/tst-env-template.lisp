(in-package :env-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

;; rules' RHS is evaluated by eval, which doesn't have acces to the local
;; environment at the time of rule creation
(defvar *env*)
(defvar *in*)
(defvar *goal*)

(defclass template-env-tests (test-case)
  ((env :accessor env :initform (make-environment))
   (t-goal :accessor tmpl-goal)
   (t-in :accessor tmpl-in)))

(defmethod set-up-move ((tests template-env-tests))
  (with-slots (env t-goal t-in) tests
    (let ((rule (make-rule
		 :move
		 (list (make-template-pattern t-goal '(:action ?action :object ?object
						     :from ?from :to ?to))
		       (make-template-pattern t-in   '(:object ?object :location ?from))
		       (make-template-pattern t-in   '(:object robot :location ?from) :negated t)
		       (make-template-pattern t-in   '(:object robot :location ?z)))
		 '((rem-fact *env* (make-template-fact *in* '(:object robot :location ?z)))
		   (add-fact *env* (make-template-fact *in* '(:object robot :location ?from)))))))
      (add-rule env rule))))

(defmethod set-up-push ((tests template-env-tests))
  (with-slots (env t-goal t-in) tests
    (let ((rule (make-rule
		 :push
		 (list (make-template-pattern t-goal '(:action ?action :object ?object
						     :from ?from :to ?to))
		       (make-template-pattern t-in   '(:object ?object :location ?from))
		       (make-template-pattern t-in   '(:object robot :location ?from)))
		 '((rem-fact *env* (make-template-fact *in* '(:object robot :location ?from)))
		   (add-fact *env* (make-template-fact *in* '(:object robot :location ?to)))
		   (rem-fact *env* (make-template-fact *in* '(:object ?object :location ?from)))
		   (add-fact *env* (make-template-fact *in* '(:object ?object :location ?to)))))))
      (add-rule env rule))))

(defmethod set-up-stop ((tests template-env-tests))
  (with-slots (env t-goal t-in) tests
    (let ((rule (make-rule
		 :stop
		 (list (make-template-pattern t-goal '(:action ?action :object ?object
						     :from ?from :to ?to))
		       (make-template-pattern t-in   '(:object ?object :location ?to)))
		 ())))
      (add-rule env rule))))

(defmethod add-facts ((tests template-env-tests))
  (with-slots (env t-in t-goal) tests
    (add-fact env (make-template-fact t-in   '(:object robot :location A)))
    (add-fact env (make-template-fact t-in   '(:object box :location B)))
    (add-fact env (make-template-fact t-goal '(:action push :object box :from B :to A)))))

(defmethod set-up ((tests template-env-tests))
  (with-slots (rete t-goal t-in) tests
    (setf t-goal (make-template :goal '(action object from to))
	  t-in   (make-template :in   '(object location)))
    (setf *env* (env tests) *in* t-in *goal* t-goal)
    (set-up-move tests)
    (set-up-push tests)
    (set-up-stop tests)
    (add-facts tests)))

(defun simulate-step (env)
  (activate-rule (select-activation env)))

(def-test-method test-template-env ((tests template-env-tests) :run nil)
  (with-slots (env t-in) tests
    (simulate-step env)
    (simulate-step env)
    (simulate-step env)
    (assert-true (find-fact env (make-template-fact t-in '(:object box :location A))))))

(add-test-suite 'template-env-tests)
;(textui-test-run (get-suite template-env-tests))
