(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :exil-utils
  (:use :common-lisp)
  (:shadow :intern :symbol-name)
  (:export :intern :string-append :symbol-name :symbol-append :to-keyword
	   :from-keyword :mac-exp :subsets :assoc-value :assoc-key :to-list
	   :to-list-of-lists :my-pushnew :ext-pushnew :ext-delete :diff-delete
	   :push-update :class-slot-value :select :weak-symbol-equal-p))

(defpackage :exil-core
  (:use :common-lisp :exil-utils)
  (:shadowing-import-from :exil-utils :intern :symbol-name)
  (:export :variable-p :template :tmpl-name :slots :make-tamplate :find-atom
	   :make-template :fact :fact-equal-p :simple-fact :find-atom :atom-position
	   :template-fact :tmpl-fact-slot-value :fact-slot :make-fact
	   :atom-equal-p :constant-test :pattern :negated-p :pattern-equal-p
	   :simple-pattern :var-or-equal-p :template-pattern :make-pattern
	   :rule :rule-equal-p :make-rule :name :conditions :activations))

(defpackage :exil-rete
  (:use :common-lisp :exil-utils :exil-core)
  (:shadowing-import-from :exil-utils :intern :symbol-name)
  (:export :add-wme :rem-wme :new-production :remove-production :make-rete
	   :token->list :token-equal-p))

(defpackage :exil-env
  (:use :common-lisp :exil-utils :exil-core :exil-rete)
  (:shadowing-import-from :exil-utils :intern :symbol-name)
  (:export :add-template :add-fact :rem-fact :reset-environment
	   :add-fact-group :add-rule :rem-fule :find-rule
	   :add-strategy :set-strategy :select-activation
	   :set-watcher :unset-watcher :watched-p :activate-rule
	   :agenda :fact-groups :find-template :rete :add-match :remove-match))

(defpackage :exil
  (:use :common-lisp :exil-utils :exil-core :exil-env)
  (:shadowing-import-from :exil-utils :intern :symbol-name)
  (:export :deftemplate :assert :retract :modify :clear :deffacts :reset
	   :defrule :undefrule :defstrategy :setstrategy :watch :unwatch
	   :step :halt :run)
  (:shadow :assert :step))

(defpackage :exil-user
  (:use :common-lisp :exil)
  (:shadowing-import-from :exil :assert :step))
