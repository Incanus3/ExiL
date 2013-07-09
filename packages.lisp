(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :exil-utils
  (:documentation "general purpose utilities used in the rest of the code")
  (:use :common-lisp :iterate)
  (:export :to-keyword :assoc-value :add-assoc-value :assoc-key
           :to-list :to-list-of-lists
           :ext-pushnew :push-end :pushnew-end :ext-delete :diff-remove
           :push-update :numbered-map :alist-equal-p :plistp :alistp
           :doplist :weak-equal-p :gensymedp
	   :map-hash-table :copy-hash-table :hash-values :hash-keys
	   :hash-equal-p :set-equal-p :partition-hash :partition))

(defpackage :exil-core
  (:documentation "core functionality of the expert system library - facts,
                   templates, patterns and rules")
  (:use :common-lisp :exil-utils :iterate)
  (:export :variable-p :template :template-name :slots
           :has-slot-p :make-template :fact :simple-fact
           :atom-position :template-fact :exil-equal-p
           :slot-default :doslots :object-slot
           :make-simple-fact :match-var :atom-equal-p
           :constant-test :pattern :make-simple-pattern
           :make-template-fact :mod-fact :make-template-pattern
           :negated-p :simple-pattern :var-or-equal-p
           :template-pattern :rule :rule-equal-p :make-rule
           :name :conditions :activations :description))

(defpackage :exil-rete
  (:documentation "the rete algorithm for matching facts against rule conditions")
  (:nicknames :erete)
  (:use :common-lisp :exil-core :iterate)
  (:import-from :exil-utils :push-update :ext-pushnew :diff-remove :map-hash-table
		:hash-values :hash-keys :to-list :set-equal-p :gensymedp :assoc-value)
  (:export :add-wme :rem-wme :new-production :remove-production :make-rete
           :token->list :token-equal-p :copy-rete :rete-copy-p))

(defpackage :exil-env
  (:documentation "the exil environment, keeps track of the defined templates
                   and rules and stores the asserted facts")
  (:nicknames :eenv)
  (:use :common-lisp :exil-core :exil-rete :iterate)
  (:import-from :exil-utils :to-keyword :assoc-value :add-assoc-value
                :ext-delete :ext-pushnew :pushnew-end :numbered-map
		:copy-hash-table :hash-equal-p :set-equal-p)
  (:export :environment :make-environment
           :set-watcher :unset-watcher :watch-all :unwatch-all :watched-p
           :add-template :find-template
           :facts :add-fact :rem-fact :find-fact
           :add-fact-group :rem-fact-group :find-fact-group
           :add-strategy :set-strategy :current-strategy-name
           :add-rule :rem-rule :find-rule
           :activations
           :clear-env :reset-env :completely-reset-env :almost-completely-reset-env
	   :undo :redo :print-undo-stack :print-redo-stack
           ;; consider if this is supposed to be environment's responsibility
           :select-activation :activate-rule
           ;; called by rete
           :add-match :remove-match))

(defpackage :exil-parser
  (:documentation "parses external representation of objects (facts, patterns,
                   templates, rules, fact-groups) into internal representation")
  (:nicknames :eparser)
  (:use :common-lisp :exil-core :iterate)
  (:import-from :exil-utils :to-keyword :to-list-of-lists :weak-equal-p :plistp
                :alistp :doplist)
  (:import-from :exil-env :environment :find-template)
  (:export :parse-template :parse-fact :modify-fact :parse-pattern
           :parse-fact-group :parse-rule))

(defpackage :exil
  (:documentation "the main package, used by exil-user")
  (:use :common-lisp :exil-parser :exil-env :iterate)
  (:import-from :exil-utils :to-keyword)
  (:export :deftemplate :assert :retract :retract-all :modify :clear :agenda
           :deffacts :undeffacts :reset :defrule :undefrule :defstrategy
           :setstrategy :watch :unwatch :step :halt :run :facts :ppdefrule
	   :undo :redo :undo-stack :redo-stack
           :complete-reset) ;; DEBUG
  (:shadow :assert :step :facts :undo :redo))

#+lispworks (defpackage :exil-gui
              (:documentation "the ExiL GUI for LispWorks")
              (:use :common-lisp :capi)
              (:import-from :exil-env :facts :templates :rules :activations
               :rem-fact :rem-rule)
              (:import-from :exil-utils :hash-values)
              (:import-from :exil :*current-environment*)
              (:export :show-gui :update-lists))

(defpackage :exil-user
  (:documentation "the user program is defined in this package")
  (:use :common-lisp :exil)
  (:shadowing-import-from :exil :assert :step))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS

(defpackage :tests-base
  (:use :common-lisp :xlunit :exil-utils)
  (:export :add-test-suite :run-suites))

(defpackage :utils-tests
  (:documentation "tests for the utils package")
  (:use :common-lisp :exil-utils :xlunit :tests-base))

(defpackage :core-tests
  (:documentation "tests for the utils package")
  (:use :common-lisp :exil-core :xlunit :tests-base))

(defpackage :rete-tests
  (:documentation "tests for the rete package")
  (:use :common-lisp :exil-utils :exil-core :exil-rete :xlunit :iterate :tests-base))

(defpackage :env-tests
  (:documentation "tests for the environment package")
  (:use :common-lisp :exil-core :exil-env :xlunit :tests-base))

(defpackage :integration-tests
  (:use :common-lisp :exil :xlunit :tests-base)
  (:shadowing-import-from :exil :assert :step :run))
