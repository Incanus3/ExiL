(require :asdf)
(defpackage :exil-system
  (:documentation "contains the ExiL ASDF system")
  (:use :asdf :cl))
(in-package :exil-system)

(defsystem exil
  :name "EXpert system In Lisp"
  :author "Jakub Kalab <jakubkalab@gmail.com>"
;  :version "0.1"
  :maintainer "Jakub Kalab <jakubkalab@gmail.com>"
;  :licence "BSD"
  :description "EXpert system In Lisp"
;  :long-description ""
  :depends-on (:xlunit :iterate)
  :components
  ((:file "packages")
   (:module :utils                       :depends-on ("packages")
            :components
            ((:file "utils")))
   (:module :core                        :depends-on (:utils)
            :components
            ((:file "templates")
             (:file "base-objects"       :depends-on ("templates"))
             (:file "patterns"           :depends-on ("facts"))
             (:file "facts"              :depends-on ("base-objects"))
             (:file "rules"              :depends-on ("patterns"))))
   (:module :rete                        :depends-on (:core)
            :components
            ((:file "tokens")
	     (:module
	      :rete-nodes                :depends-on ("tokens")
	      :components
	      ((:file "generic-node")
	       (:file "alpha-top"        :depends-on ("generic-node"))
	       (:file "alpha-tests-mems" :depends-on ("alpha-top"))
	       (:file "beta-tests"       :depends-on ("alpha-tests-mems"))
	       (:file "beta-memories"    :depends-on ("beta-tests"))
	       (:file "beta-joins"       :depends-on ("beta-memories"))))
	     (:file "rete-class"         :depends-on (:rete-nodes))
             (:file "create-alpha-net"   :depends-on ("rete-class"))
             (:file "create-beta-net"    :depends-on ("create-alpha-net"))
	     (:file "rete-print"         :depends-on ("create-beta-net"))
	     (:file "graph-traversal"    :depends-on ("rete-print"))
	     (:file "rete-equal"         :depends-on ("graph-traversal"))
	     (:file "rete-copy"          :depends-on ("rete-equal"))
	     ))
   (:module :environment                 :depends-on (:rete)
            :components
            ((:file "matches")
             (:file "activations"        :depends-on ("matches"))
             (:file "strategies"         :depends-on ("activations"))
             (:file "env-base"           :depends-on ("strategies"))
	     (:file "env-slots"          :depends-on ("env-base"))
	     (:file "env-undo"           :depends-on ("env-slots"))
	     (:file "env-watchers"       :depends-on ("env-undo"))
             (:file "env-facts"          :depends-on ("env-watchers"))
             (:file "env-activations"    :depends-on ("env-facts"))))
   (:module :parser                      :depends-on (:environment)
            :components
            ((:file "prs-base")
             (:file "prs-templates"      :depends-on ("prs-base"))
             (:file "prs-facts"          :depends-on ("prs-templates"))
             (:file "prs-rules"          :depends-on ("prs-facts"))))
   (:module :front-end                   :depends-on (:parser)
            :components
            ((:file "fe-base")
             (:file "fe-facts"           :depends-on ("fe-base"))
             (:file "fe-rules"           :depends-on ("fe-facts"))
             (:file "fe-execution"       :depends-on ("fe-rules"))))
   (:module :tests                       :depends-on (:front-end)
            :components
            ((:file "tst-base")
             (:file "tst-utils"          :depends-on ("tst-base"))
             (:file "tst-templates"      :depends-on ("tst-utils"))
             (:file "tst-base-objects"   :depends-on ("tst-templates"))
             (:file "tst-patterns"       :depends-on ("tst-base-objects"))
	     (:module
	      :rete                      :depends-on ("tst-patterns")
	      :components
	      ((:file "tst-tokens")
	       (:file "tst-rete"         :depends-on ("tst-tokens"))
	       (:file "tst-rete-walk"    :depends-on ("tst-rete"))
	       (:file "tst-rete-copy"    :depends-on ("tst-rete-walk"))))
             (:file "tst-environment"    :depends-on (:rete))
	     (:file "tst-undo"           :depends-on ("tst-environment"))
	     (:file "tst-undo2"          :depends-on ("tst-undo"))
             (:file "run-tests"          :depends-on ("tst-undo2"))))
   (:module :examples                    :depends-on (:front-end)
            :components
            ((:file "examples-template")
             (:file "examples-simple")
             (:file "examples-clips")))
   #+lispworks(:file "gui"     :depends-on (:front-end))
;   (:file "pokusy"            :depends-on ("export"))
   )
;  :properties ((:author-email . "jakubkalab@gmail.com")
;               (:date . "4.9.2010")
  )
