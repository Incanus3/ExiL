(in-package :exil-user)

(princ "========================================================================")

(complete-reset)

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

(back-run)

(defvar env exil::*current-environment*)

(format t "~%~%AFTER FIRST RUN:")
(format t "~%GOALS: ~A" (eenv::goals env))
(format t "~%SUBSTITUTIONS: ~A" (eenv::all-used-substitutions env))

(back-run)

(format t "~%~%AFTER SECOND RUN:")
(format t "~%GOALS: ~A" (eenv::goals env))
(format t "~%SUBSTITUTIONS: ~A" (eenv::all-used-substitutions env))
