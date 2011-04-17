(in-package :cl-user)

(defpackage :exil
  (:use :common-lisp)
  (:export :deftemplate :assert :retract :modify :clear :deffacts :reset
	   :defrule :undefrule :defstrategy :set-strategy :step :halt :run
	   :watch :unwatch)
  (:shadow :assert :intern :symbol-name :step))