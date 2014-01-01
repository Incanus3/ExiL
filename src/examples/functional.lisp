(in-package :exil-user)

(princ "======================================================================")

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

;; backward chaining can't work here, because it has no way of predicting the
;; assertf result
(format t "~%~%BACKWARD:")
(defgoal (palindrome c b a b c))
(back-run)

;; forward chaining works, but it just repeats the first rule over and over
(format t "~%~%FORWARD:")
(step)
(step)
(step)
