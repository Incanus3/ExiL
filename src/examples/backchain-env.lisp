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
(back-run env)

OUTPUT:
All goals have been satisfied
(MOTHER-OF ?MOTHER-OF-GEORGE GEORGE) satisfied by (RULE MOTHER
  (FEMALE ?MOTHER)
  (PARENT-OF ?MOTHER ?CHILD)
  =>
  (ASSERT (MOTHER-OF ?MOTHER ?CHILD)))
(FEMALE ?MOTHER) satisfied by (FEMALE JANE)
(PARENT-OF JANE GEORGE) satisfied by (PARENT-OF JANE GEORGE)
These variable bindings have been used:
((?MOTHER-OF-GEORGE . JANE))
|#
