;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defpackage :exil-test
  (:use :common-lisp)
  (:shadowing-import-from :exil-utils :intern :string-append :symbol-name
  :symbol-append :to-keyword :from-keyword :mac-exp :subsets :assoc-value
  :assoc-key :to-list :to-list-of-lists :my-pushnew :ext-pushnew :ext-delete
  :diff-delete :push-update :class-slot-value :select)
  (:shadowing-import-from :exil-core :variable-p :template :tmpl-slot-spec
  :tmpl-equal-p :make-template :template-object :tmpl-object-slot-value
  :tmpl-object-equal-p :find-atom :atom-position :find-template
  :make-tmpl-object :tmpl-object-specification-p :fact :fact-equal-p
  :simple-fact :template-fact :tmpl-fact-slot-value :fact-slot :make-tmpl-fact
  :tmpl-fact-specification-p :make-fact :atom-equal-p :constant-test :pattern
  :pattern-equal-p :simple-pattern :var-or-equal-p :pattern-const-equal-p
  :template-pattern :tmpl-pattern-slot-value :pattern-slot :make-tmpl-pattern
  :tmpl-pattern-specification-p :make-pattern :rule :rule-equal-p :make-rule)
  (:shadowing-import-from :exil-rete :token :empty-token :previous-wme
  :includes-p :token-equal-p :token->list :described-object :node :node-equal-p
  :add-child :add-children :activate :activate-children :inactivate
  :inactivate-children :memory-node :add-item :alpha-node :alpha-test-node :test
  :activate-memory :simple-fact-alpha-node :template-fact-alpha-node
  :simple-fact-test-node :template-fact-test-node :alpha-subtop-node
  :simple-fact-subtop-node :template-fact-subtop-node :alpha-top-node
  :get-network :initialize-network :get/initialize-network :alpha-memory-node
  :beta-node :beta-memory-node :agenda :add-match :remove-match :complete-match
  :broken-match :add-production :delete-production :beta-top-node :make-test
  :test-equal-p :tests-equal-p :beta-join-node :beta-memory :perform-join-test
  :perform-join-tests :beta-negative-node :get-bad-wmes :rete :add-wme :rem-wme
  :make-rete :find-test-node :find/create-test-node% :find/create-test-node
  :create-alpha-net% :create-alpha-net :find-atom-in-cond-list%
  :get-intercondition-tests% :get-intracondition-tests%
  :get-join-tests-from-condition :find/create-join-node :find/create-neg-node
  :new-production :remove-production)
  (:shadowing-import-from :exil-env :match :make-match :match-equal-p
  :variable-bindings :get-variable-bindings :substitute-variables :activate-rule
  :newer-than :depth-strategy :breadth-strategy :simpler-than
  :simplicity-strategy :complexity-strategy :exil-environment :*environments*
  :defenv :setenv :*current-environment* :exil-env-reader :,slot-name
  :exil-env-writer :exil-env-accessor :exil-env-accessors :facts :add-fact
  :rem-fact :add-fact-group :add-template :find-template :add-rule
  :remove-matches :rem-rule :find-rule :add-match :remove-match :add-strategy
  :set-strategy :current-strategy :select-activation :set-watcher :unset-watcher
  :watched-p :reset-environment :completely-reset-environment)
  (:shadowing-import-from :exil-frontend :deftemplate :assert% :assert :retract%
  :retract :modify% :modify :clear :deffacts :assert-group% :reset :defrule
  :undefrule :defstrategy :setstrategy :step :*exil-running* :halt :run :watch
  :unwatch))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
