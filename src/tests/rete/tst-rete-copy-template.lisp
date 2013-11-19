(in-package :rete-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass template-rete-copy-tests (test-case)
  ((t-goal :accessor tmpl-goal)
   (t-in :accessor tmpl-in)
   (rete :accessor rete)))

(defmethod set-up-move ((tests template-rete-copy-tests))
  (with-slots (t-goal t-in rete) tests
    (let ((move (make-rule
		 :move
		 (list (make-template-pattern t-goal '(:action ?action :object ?object
						     :from ?from :to ?to))
		       (make-template-pattern t-in   '(:object ?object :location ?from))
		       (make-template-pattern t-in   '(:object robot :location ?from) :negated t)
		       (make-template-pattern t-in   '(:object robot :location ?z)))
		 ())))
      (new-production rete move))))

(defmethod set-up-push ((tests template-rete-copy-tests))
  (with-slots (rete t-goal t-in) tests
    (let ((move (make-rule
		 :push
		 (list (make-template-pattern t-goal '(:action ?action :object ?object
						     :from ?from :to ?to))
		       (make-template-pattern t-in   '(:object ?object :location ?from))
		       (make-template-pattern t-in   '(:object robot :location ?from)))
		 ())))
      (new-production rete move))))

(defmethod set-up-stop ((tests template-rete-copy-tests))
  (with-slots (rete t-goal t-in) tests
    (let ((move (make-rule
		 :push
		 (list (make-template-pattern t-goal '(:action ?action :object ?object
						     :from ?from :to ?to))
		       (make-template-pattern t-in   '(:object ?object :location ?to)))
		 ())))
      (new-production rete move))))

(defmethod add-facts ((tests template-rete-copy-tests))
  (with-slots (rete t-in t-goal) tests
    (add-wme rete (make-template-fact t-in   '(:object robot :location A)))
    (add-wme rete (make-template-fact t-in   '(:object box :location B)))
    (add-wme rete (make-template-fact t-goal '(:action push :object box :from B :to A)))))

(defmethod set-up ((tests template-rete-copy-tests))
  (with-slots (rete t-goal t-in) tests
    (setf rete (make-rete (make-instance 'env-mock))
	  t-goal (make-template :goal '(action object from to))
	  t-in   (make-template :in   '(object location)))
    (set-up-move tests)
    (set-up-push tests)
    (set-up-stop tests)
    (add-facts tests)))

(def-test-method test-copy-rete ((tests template-rete-copy-tests) :run nil)
  (with-slots (rete) tests
    (let* ((new-env (make-instance 'env-mock))
	   (new-rete (copy-rete rete new-env)))
      (format t "~%testing equivalency of ~S nodes" (length (erete::rete-nodes rete)))
      (assert-true (erete::rete-copy-p rete new-rete))
      (assert-false (intersection (erete::rete-nodes rete)
				  (erete::rete-nodes new-rete))))))

(add-test-suite 'template-rete-copy-tests)
;(textui-test-run (get-suite template-rete-copy-tests))
