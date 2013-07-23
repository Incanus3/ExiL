(in-package :exil-env)

(defparameter env (make-environment))

(add-fact env (make-simple-fact '(female jane)))
(add-fact env (make-simple-fact '(parent-of jane george)))

(add-rule env (make-rule
	       :mother
	       (list (make-simple-pattern '(female ?mother))
		     (make-simple-pattern '(parent-of ?mother ?child)))
	       (list '(assert (mother-of ?mother ?child)))))

(add-goal env (make-simple-pattern '(mother-of ?mother-of-george george)))

#|
(goals env) ;=> ((mother-of ?mother george))
(back-step env)
(goals env) ;=> ((female ?mother) (parent-of ?mother george))
(back-step env)
(goals env) ;=> ((parent-of jane george))
(back-step env)
(goals env) ;=> ()
|#
