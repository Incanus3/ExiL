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
   (:module :utils
            :depends-on ("packages")
            :components
            ((:file "utils")))
   (:module :core
            :depends-on ("packages" :utils)
            :components
            ((:file "templates")
             (:file "base-objects"       :depends-on ("templates"))
             (:file "patterns"           :depends-on ("facts"))
             (:file "facts"              :depends-on ("base-objects"))
             (:file "rules"              :depends-on ("patterns"))))
   (:module :rete
            :depends-on ("packages" :utils :core)
            :components
            ((:file "tokens")
             (:file "generic-node"       :depends-on ("tokens"))
             (:file "alpha-part"         :depends-on ("generic-node"))
             (:file "beta-memories"      :depends-on ("alpha-part"))
             (:file "beta-joins"         :depends-on ("beta-memories"))
             (:file "create-alpha-net"   :depends-on ("beta-joins"))
             (:file "create-beta-net"    :depends-on ("create-alpha-net"))))
   (:module :environment
            :depends-on ("packages" :utils :core :rete)
            :components
            ((:file "matches")
             (:file "activations"        :depends-on ("matches"))
             (:file "strategies"         :depends-on ("activations"))
             (:file "env-base"           :depends-on ("strategies"))
	     (:file "env-watchers"       :depends-on ("env-base"))
             (:file "env-facts"          :depends-on ("env-watchers"))
             (:file "env-activations"    :depends-on ("env-facts"))))
   (:module :parser
            :depends-on ("packages" :utils :core :environment)
            :components
            ((:file "prs-base")
             (:file "prs-templates" :depends-on ("prs-base"))
             (:file "prs-facts" :depends-on ("prs-templates"))
             (:file "prs-rules" :depends-on ("prs-facts"))))
   (:module :front-end
            :depends-on ("packages" :utils :environment :parser)
            :components
            ((:file "fe-base")
             (:file "fe-facts" :depends-on ("fe-base"))
             (:file "fe-rules" :depends-on ("fe-facts"))
             (:file "fe-execution" :depends-on ("fe-rules"))))
   (:module :tests
            :depends-on ("packages" :utils :core :rete :environment
                                    :parser :front-end)
            :components
            ((:file "tst-base")
             (:file "tst-utils"        :depends-on ("tst-base"))
             (:file "tst-templates"    :depends-on ("tst-utils"))
             (:file "tst-base-objects" :depends-on ("tst-templates"))
             (:file "tst-patterns"     :depends-on ("tst-base-objects"))
             (:file "tst-tokens"       :depends-on ("tst-patterns"))
             (:file "tst-rete"         :depends-on ("tst-tokens"))
             (:file "tst-environment"  :depends-on ("tst-rete"))
	     (:file "tst-undo"         :depends-on ("tst-environment"))
             (:file "run-tests"    :depends-on ("tst-undo"))))
   (:module :examples
            :depends-on ("packages" :front-end)
            :components
            ((:file "examples-template")
             (:file "examples-simple")
             (:file "examples-clips")))
   #+lispworks(:file "gui"     :depends-on (:front-end))
   #|
   (:file "print-tree"        :depends-on ("export"))
   (:file "pokusy"            :depends-on ("export"))
   |#
   )
;  :properties ((:author-email . "jakubkalab@gmail.com")
;               (:date . "4.9.2010")
  )
