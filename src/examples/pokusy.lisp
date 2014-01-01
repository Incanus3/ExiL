(in-package :exil-user)

(princ "========================================================================")

(complete-reset)
(unwatch all)
(watch facts)

(deffacts world
  (palindrome "a")
  (palindrome "b")
  (palindrome "c"))

(defrule surround-by-as
  (palindrome ?p)
  =>
  (assertf `(palindrome ,(concatenate 'string "a" ?p "a"))))

(defrule surround-by-as
  (palindrome ?p)
  =>
  (assertf `(palindrome ,(concatenate 'string "a" ?p "a"))))

(defrule surround-by-as
  (palindrome ?p)
  =>
  (assertf `(palindrome ,(concatenate 'string "a" ?p "a"))))

(reset)
(defgoal (rent-payed))

(format t "~%~%BACKWARD:")
(defgoal (palindrome c b a b c))
(back-run)

(format t "~%~%FORWARD:")
(step)
(step)
(step)

;; lisp doesn't provide backquote as macro, otherwise assert and retract could
;; backquote the specifiers instead of just quoting them, allowing for comma
;; interpolation

;; (defvar env exil::*current-environment*)

;; (format t "~%~%AFTER FIRST RUN:")
;; (format t "~%GOALS: ~A" (eenv::goals env))
;; (format t "~%SUBSTITUTIONS: ~A" (eenv::all-used-substitutions env))

;; (back-run)

;; (format t "~%~%AFTER SECOND RUN:")
;; (format t "~%GOALS: ~A" (eenv::goals env))
;; (format t "~%SUBSTITUTIONS: ~A" (eenv::all-used-substitutions env))
