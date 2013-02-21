(require :asdf)
(defpackage :exil-system
  (:documentation "contains the ExiL ASDF system")
  (:use :asdf :cl))
(in-package :exil-system)

(defsystem exil
  :name "EXpert system In Lisp"
  :author "Jakub Kalab <jakubkalab@gmail.com>"
  :version "0.1"
  :maintainer "Jakub Kalab <jakubkalab@gmail.com>"
  :licence "BSD"
  :description "EXpert system In Lisp"
  :long-description ""
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
             (:file "env-facts"          :depends-on ("env-base"))
             (:file "env-activations"    :depends-on ("env-facts"))))
   (:module :parser
            :depends-on ("packages" :utils :core :environment)
            :components
            ((:file "base")
             (:file "templates" :depends-on ("base"))
             (:file "facts" :depends-on ("templates"))
             (:file "rules" :depends-on ("facts"))))
   (:module :front-end
            :depends-on ("packages" :utils :environment :parser)
            :components
            ((:file "base")
             (:file "facts" :depends-on ("base"))
             (:file "rules" :depends-on ("facts"))
             (:file "execution" :depends-on ("rules"))))
   (:module :tests
            :depends-on ("packages" :utils :core :rete :environment
                                    :parser :front-end)
            :components
            ((:file "base")
             (:file "utils"        :depends-on ("base"))
             (:file "templates"    :depends-on ("base"))
             (:file "base-objects" :depends-on ("base"))
             (:file "patterns"     :depends-on ("base"))
             (:file "tokens"       :depends-on ("base"))
             (:file "rete"         :depends-on ("base"))
             (:file "environment"  :depends-on ("base"))
             (:file "run-tests"
                    :depends-on ("base" "utils" "templates" "base-objects" "rete"
                                        "patterns" "tokens" "environment"))))
   (:module :examples
            :depends-on ("packages" :front-end)
            :components
            ((:file "examples")
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
