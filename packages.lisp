(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :exil-utils
  (:documentation "general purpose utilities used in the rest of the code")
  (:use :common-lisp :iterate)
  (:shadow :intern :symbol-name)
  (:export :intern :string-append :symbol-name :symbol-append :to-keyword
           :from-keyword :mac-exp :subsets :assoc-value :assoc-key :to-list
           :to-list-of-lists :my-pushnew :ext-pushnew :push-end :pushnew-end
           :ext-delete :diff-remove :push-update :select :alist-equal-p
           :cpl-assoc-val :plistp :alistp :plist-every :plist-equal-p :rgetf
           :doplist :weak-equal-p :hash->list :symbol-name-equal-p))

(defpackage :exil-core
  (:documentation "core functionality of the expert system library - facts,
                   templates, patterns and rules")
  (:use :common-lisp :exil-utils :iterate)
  (:shadowing-import-from :exil-utils :intern :symbol-name)
  (:export :variable-p :template :slots :find-atom
           :has-slot-p :make-template :fact :simple-fact
           :atom-position :template-fact :exil-equal-p
           :slot-default :doslots :copy-object :object-slot
           :make-simple-fact :match-var :atom-equal-p
           :constant-test :pattern :make-simple-pattern
           :make-template-fact :make-template-pattern
           :negated-p :simple-pattern :var-or-equal-p
           :template-pattern :rule :rule-equal-p :make-rule
           :name :conditions :activations :description))

(defpackage :exil-rete
  (:documentation "the rete algorithm for matching facts against rule conditions")
  (:nicknames :erete)
  (:use :common-lisp :exil-utils :exil-core :iterate)
  (:shadowing-import-from :exil-utils :intern :symbol-name)
  (:export :add-wme :rem-wme :new-production :remove-production :make-rete
           :token->list :token-equal-p))

(defpackage :exil-env
  (:documentation "the exil environment, keeps track of the defined templates
                   and rules and stores the asserted facts")
  (:nicknames :eenv)
  (:use :common-lisp :exil-utils :exil-core :exil-rete :iterate)
  (:shadowing-import-from :exil-utils :intern :symbol-name)
  (:export :environment :make-environment
           :set-watcher :unset-watcher :watch-all :unwatch-all
           :add-template :find-template
           :facts :add-fact :rem-fact :find-fact ;<- remove this
           :reset-facts ;<- use reset-env
;           :modify-fact ;<- move this to parser
           :add-fact-group :rem-fact-group ;:fact-groups
           :add-strategy :set-strategy
           :add-rule :rem-rule :find-rule
           :agenda
           :clear-env :reset-env :completely-reset-env
           ;; consider if this is supposed to be environment's responsibility
           :select-activation :activate-rule
           :make-fact :make-pattern ; move this to parser
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
  (:export :parse-template :parse-fact :modify-fact
           :parse-fact-group :parse-rule))

(defpackage :exil
  (:documentation "the main package, used by exil-user")
  (:use :common-lisp :exil-parser :exil-env :iterate)
  (:import-from :exil-utils :to-keyword)
;; should be removed when rule printing moved to core
  (:import-from :exil-core :conditions :activations)
  (:export :deftemplate :assert :retract :retract-all :modify :clear :agenda
           :deffacts :undeffacts :reset :defrule :undefrule :defstrategy
           :setstrategy :watch :unwatch :step :halt :run :facts :ppdefrule
           :complete-reset) ;; DEBUG
  (:shadow :assert :step :facts))

#+lispworks (defpackage :exil-gui
              (:documentation "the ExiL GUI for LispWorks")
              (:use :common-lisp :capi)
              (:import-from :exil-env :facts :templates :rules :agenda
               :rem-fact :rem-rule)
              (:import-from :exil-utils :hash->list)
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
  (:shadowing-import-from :exil-utils :intern :symbol-name)
  (:export :add-test-suite :run-suites))

(defpackage :utils-tests
  (:documentation "tests for the utils package")
  (:use :common-lisp :exil-utils :xlunit)
  (:import-from :tests-base :add-test-suite)
  (:shadowing-import-from :exil-utils :intern :symbol-name))

(defpackage :core-tests
  (:documentation "tests for the utils package")
  (:use :common-lisp :exil-core :xlunit)
  (:import-from :tests-base :add-test-suite))

(defpackage :rete-tests
  (:documentation "tests for the rete package")
  (:use :common-lisp :exil-core :exil-rete :xlunit)
  (:import-from :tests-base :add-test-suite))

(defpackage :env-tests
  (:documentation "tests for the environment package")
  (:use :common-lisp :exil-core :exil-env :xlunit)
  (:import-from :tests-base :add-test-suite))
