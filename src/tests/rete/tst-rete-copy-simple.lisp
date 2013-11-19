(in-package :rete-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass simple-rete-copy-tests (test-case)
  ((rete :accessor rete)))

(defgeneric set-up-move (tests))
(defgeneric set-up-push (tests))
(defgeneric set-up-stop (tests))
(defgeneric add-facts (tests))

(defmethod set-up-move ((tests simple-rete-copy-tests))
  (with-slots (rete) tests
    (let ((rule (make-rule
		 :move
		 (list (make-simple-pattern '(goal ?action ?object ?from ?to))
		       (make-simple-pattern '(in ?object ?from))
		       (make-simple-pattern '(in robot ?from) :negated t)
		       (make-simple-pattern '(in robot ?z)))
		 ())))
      (new-production rete rule))))

(defmethod set-up-push ((tests simple-rete-copy-tests))
  (with-slots (rete) tests
    (let ((rule (make-rule
		 :push
		 (list (make-simple-pattern '(goal ?action ?object ?from ?to))
		       (make-simple-pattern '(in ?object ?from))
		       (make-simple-pattern '(in robot ?from)))
		 ())))
      (new-production rete rule))))

(defmethod set-up-stop ((tests simple-rete-copy-tests))
  (with-slots (rete) tests
    (let ((rule (make-rule
		 :stop
		 (list (make-simple-pattern '(goal ?action ?object ?from ?to))
		       (make-simple-pattern '(in ?object ?to)))
		 ())))
      (new-production rete rule))))

(defmethod add-facts ((tests simple-rete-copy-tests))
  (with-slots (rete) tests
    (add-wme rete (make-simple-fact '(in robot A)))
    (add-wme rete (make-simple-fact '(in box B)))
    (add-wme rete (make-simple-fact '(goal push box B A)))))

(defmethod set-up ((tests simple-rete-copy-tests))
  (setf (rete tests) (make-rete (make-instance 'env-mock)))
  (set-up-move tests)
  (set-up-push tests)
  (set-up-stop tests)
  (add-facts tests))

(def-test-method test-copy-rete ((tests simple-rete-copy-tests) :run nil)
  (with-slots (rete) tests
    (let* ((new-env (make-instance 'env-mock))
	   (new-rete (copy-rete rete new-env)))
      (format t "~%testing equivalency of ~S nodes" (length (erete::rete-nodes rete)))
      (assert-true (erete::rete-copy-p rete new-rete))
      (assert-false (intersection (erete::rete-nodes rete)
				  (erete::rete-nodes new-rete))))))

(add-test-suite 'simple-rete-copy-tests)
;(textui-test-run (get-suite simple-rete-copy-tests))
