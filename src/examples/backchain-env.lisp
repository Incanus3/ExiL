(in-package :exil-env)

(defparameter env (make-environment))

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

#|
(print-goals env)

(back-run env)
|#
