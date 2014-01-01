(in-package :exil-user)

(princ "========================================================================")

(complete-reset)
(unwatch all)
(watch facts)

(deftemplate parent parent child)
(deftemplate father father child)
(deftemplate mother mother child)

(deffacts world
  (female jane)
  (male john)
  (parent :parent jane :child george)
  (parent :parent john :child george))

(defrule father-is-male-parent
  (male ?father)
  (parent :parent ?father :child ?child)
  =>
  (assert (father :father ?father :child ?child)))

(defrule mother-is-female-parent
  (female ?mother)
  (parent :parent ?mother :child ?child)
  =>
  (assert (mother :mother ?mother :child ?child)))

(reset)

(defgoal (mother :mother ?mother-of-george :child george))

(format t "~%~%BACKWARD:")
(back-run)

(format t "~%~%FORWARD:")
(run)
