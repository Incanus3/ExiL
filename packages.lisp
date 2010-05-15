(in-package :cl-user)

(defpackage :exil
  (:use :common-lisp)
  (:export :assert :retract :deffacts :deftemplate :defrule)
  (:shadow :assert :intern :symbol-name))