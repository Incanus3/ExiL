(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :exil-utils
  (:documentation "general purpose utilities used in the rest of the code")
  (:use :common-lisp)
  (:shadow :intern :symbol-name)
  (:export :intern :string-append :symbol-name :symbol-append :to-keyword
	   :from-keyword :mac-exp :subsets :assoc-value :assoc-key :to-list
	   :to-list-of-lists :my-pushnew :ext-pushnew :push-end :pushnew-end
	   :ext-delete :diff-delete :push-update :class-slot-value :select
	   :weak-symbol-equal-p :every-couple :cpl-assoc-val :plistp :alistp
	   :doplist :exil-equal-p :exil-weak-equal-p :hash->list))

(defpackage :utils-tests
  (:documentation "tests for the utils package")
  (:use :common-lisp :exil-utils :lift)
  (:shadowing-import-from :exil-utils :intern :symbol-name))

(defpackage :exil-core
  (:documentation "core functionality of the expert system library - facts,
                   templates, patterns and rules")
  (:use :common-lisp :exil-utils)
  (:shadowing-import-from :exil-utils :intern :symbol-name)
  (:export :variable-p :template :tmpl-name :slots :find-atom
	   :has-slot-p :make-template :fact :fact-equal-p :simple-fact
	   :atom-position :template-fact :tmpl-fact-slot-value :fact-slot
	   :make-fact :match-var :atom-equal-p :constant-test :pattern
	   :negated-p :pattern-equal-p :simple-pattern :var-or-equal-p
	   :template-pattern :make-pattern :rule :rule-equal-p :make-rule
	   :name :conditions :activations :fact-description :copy-fact))

(defpackage :core-tests
  (:documentation "tests for the utils package")
  (:use :common-lisp :exil-core :lift))

(defpackage :exil-rete
  (:documentation "the rete algorithm for matching facts against rule conditions")
  (:use :common-lisp :exil-utils :exil-core)
  (:shadowing-import-from :exil-utils :intern :symbol-name)
  (:export :add-wme :rem-wme :new-production :remove-production :make-rete
	   :token->list :token-equal-p))

(defpackage :exil-env
  (:documentation "the exil environment, keeps track of the defined templates
                   and rules and stores the asserted facts")
  (:use :common-lisp :exil-utils :exil-core :exil-rete)
  (:shadowing-import-from :exil-utils :intern :symbol-name)
  (:export :add-template :add-fact :rem-fact :reset-environment :reset-facts
	   :add-fact-group :rem-fact-group :add-rule :rem-rule :find-rule
	   :add-strategy :set-strategy :select-activation :find-fact :modify-fact
	   :set-watcher :unset-watcher :watched-p :watch-all :unwatch-all :activate-rule
	   :facts :rules :templates :agenda :fact-groups :find-template :rete
           :add-match :remove-match))

(defpackage :exil
  (:documentation "the main package, used by exil-user")
  (:use :common-lisp :exil-utils :exil-core :exil-env)
  (:shadowing-import-from :exil-utils :intern :symbol-name)
  (:export :deftemplate :assert :retract :retract-all :modify :clear :agenda
	   :deffacts :undeffacts :reset :defrule :undefrule :defstrategy
	   :setstrategy :watch :unwatch :step :halt :run :facts :ppdefrule)
  (:shadow :assert :step :facts))

#+lispworks (defpackage :exil-gui
              (:documentation "the ExiL GUI for LispWorks")
              (:use :common-lisp :capi)
              (:import-from :exil-env :facts :templates :rules :agenda :rem-fact :rem-rule)
              (:import-from :exil-utils :hash->list)
              (:export :show-gui :update-lists))

(defpackage :exil-user
  (:documentation "the user program is defined in this package")
  (:use :common-lisp :exil)
  (:shadowing-import-from :exil :assert :step))
