(in-package :exil-user)

(princ "========================================================================")

(complete-reset)
(unwatch all)
(watch facts)

(deffacts world
  (palindrome a)
  (palindrome b)
  (palindrome c))

(defrule surround-by-as
  (palindrome ?p)
  =>
  (assert (palindrome a ?p a)))

(defrule surround-by-bs
  (palindrome ?p)
  =>
  (assert (palindrome b ?p b)))

(defrule surround-by-cs
  (palindrome ?p)
  =>
  (assert (palindrome c ?p c)))

(reset)
(defgoal (rent-payed))

(format t "~%~%BACKWARD:")
(defgoal (palindrome c b a b c))
(back-run)

(format t "~%~%FORWARD:")
(step)
(step)
(step)



;; (defvar env exil::*current-environment*)

;; (format t "~%~%AFTER FIRST RUN:")
;; (format t "~%GOALS: ~A" (eenv::goals env))
;; (format t "~%SUBSTITUTIONS: ~A" (eenv::all-used-substitutions env))

;; (back-run)

;; (format t "~%~%AFTER SECOND RUN:")
;; (format t "~%GOALS: ~A" (eenv::goals env))
;; (format t "~%SUBSTITUTIONS: ~A" (eenv::all-used-substitutions env))
