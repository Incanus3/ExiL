(in-package :rete-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass simple-rete-copy-tests (test-case)
  ((rete :accessor rete)))

(defmethod set-up-move ((tests simple-rete-copy-tests))
  (with-slots (rete) tests
    (let ((move (make-rule
		 :move
		 (list (make-simple-pattern '(goal ?action ?object ?from ?to))
		       (make-simple-pattern '(in ?object ?from))
		       (make-simple-pattern '(- in robot ?from))
		       (make-simple-pattern '(in robot ?z)))
		 ())))
      (new-production rete move))))

(defmethod set-up-push ((tests simple-rete-copy-tests))
  (with-slots (rete) tests
    (let ((move (make-rule
		 :push
		 (list (make-simple-pattern '(goal ?action ?object ?from ?to))
		       (make-simple-pattern '(in ?object ?from))
		       (make-simple-pattern '(in robot ?from)))
		 ())))
      (new-production rete move))))

(defmethod set-up-stop ((tests simple-rete-copy-tests))
  (with-slots (rete) tests
    (let ((move (make-rule
		 :push
		 (list (make-simple-pattern '(goal ?action ?object ?from ?to))
		       (make-simple-pattern '(in ?object ?to)))
		 ())))
      (new-production rete move))))

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
      (assert-true (erete::rete-copy-p rete new-rete))
      (assert-false (intersection (erete::rete-nodes rete)
				  (erete::rete-nodes new-rete))))))

(add-test-suite 'simple-rete-copy-tests)
;(textui-test-run (get-suite simple-rete-copy-tests))

;; (defclass template-rete-copy-tests (test-case)
;;   ((goal :accessor goal)
;;    (in :accessor in)
;;    (rete :accessor rete)))

;; (defmethod set-up-move ((tests template-rete-copy-tests))
;;   (with-slots (goal in rete) tests
;;     (let ((move (make-rule
;; 		 :move
;; 		 (list (make-template-pattern goal '(:action ?action :object ?object ?from ?to))
;; 		       (make-template-pattern '(in ?object ?from))
;; 		       (make-template-pattern '(- in robot ?from))
;; 		       (make-template-pattern '(in robot ?z)))
;; 		 ())))
;;       (new-production rete move))))

;; (defmethod set-up-push ((tests template-rete-copy-tests))
;;   (with-slots (rete) tests
;;     (let ((move (make-rule
;; 		 :push
;; 		 (list (make-template-pattern '(goal ?action ?object ?from ?to))
;; 		       (make-template-pattern '(in ?object ?from))
;; 		       (make-template-pattern '(in robot ?from)))
;; 		 ())))
;;       (new-production rete move))))

;; (defmethod set-up-stop ((tests template-rete-copy-tests))
;;   (with-slots (rete) tests
;;     (let ((move (make-rule
;; 		 :push
;; 		 (list (make-template-pattern '(goal ?action ?object ?from ?to))
;; 		       (make-template-pattern '(in ?object ?to)))
;; 		 ())))
;;       (new-production rete move))))

;; (defmethod add-facts ((tests template-rete-copy-tests))
;;   (with-slots (rete) tests
;;     (add-wme rete (make-template-fact '(in robot A)))
;;     (add-wme rete (make-template-fact '(in box B)))
;;     (add-wme rete (make-template-fact '(goal push box B A)))))

;; (defmethod set-up ((tests template-rete-copy-tests))
;;   (with-slots (goal in rete) tests
;;     (setf rete (make-rete (make-instance 'env-mock))
;; 	  goal (make-template :goal '(action object from to))
;; 	  in (make-template :in '(object location)))
;;     (set-up-move tests)
;;     (set-up-push tests)
;;     (set-up-stop tests)
;;     (add-facts tests)))

;; (def-test-method test-copy-rete ((tests template-rete-copy-tests) :run t)
;;   (with-slots (rete) tests
;;     (let* ((new-env (make-instance 'env-mock))
;; 	   (new-rete (copy-rete rete new-env)))
;;       (assert-true (erete::rete-copy-equal-p rete new-rete))
;;       (assert-false (intersection (erete::rete-nodes rete)
;; 				  (erete::rete-nodes new-rete))))))

;; (add-test-suite 'template-rete-copy-tests)
;; ;(textui-test-run (get-suite template-rete-copy-tests))

