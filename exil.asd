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
  ((:module :base
            :components
            ((:file "packages")
             (:file "utils"              :depends-on ("packages"))
             (:file "tests"              :depends-on ("utils"))
             (:file "utils-tests"        :depends-on ("utils" "tests"))))
   (:module :core
            :depends-on (:base)
            :components
            ((:file "templates")
             (:file "templates-tests"    :depends-on ("templates"))
             (:file "base-objects"       :depends-on ("templates"))
             (:file "base-objects-tests" :depends-on ("base-objects"))
             (:file "patterns"           :depends-on ("facts"))
             (:file "patterns-tests"     :depends-on ("patterns"))
             (:file "facts"              :depends-on ("base-objects"))
             (:file "rules"              :depends-on ("patterns"))))
   (:Module :rete
            :depends-on (:core)
            :components
            ((:file "tokens")
             (:file "tokens-tests"       :depends-on ("tokens"))
             (:file "generic-node"       :depends-on ("tokens"))
             (:file "alpha-part"         :depends-on ("generic-node"))
             (:file "beta-memories"      :depends-on ("alpha-part"))
             (:file "beta-joins"         :depends-on ("beta-memories"))
             (:file "create-alpha-net"   :depends-on ("beta-joins"))
             (:file "create-beta-net"    :depends-on ("create-alpha-net"))))
   (:module :environment
            :depends-on (:rete)
            :components
            ((:file "matches")
             (:file "activations"        :depends-on ("matches"))
             (:file "strategies"         :depends-on ("activations"))
             (:file "environment"        :depends-on ("strategies"))
             (:file "object-makers"      :depends-on ("environment"))))
   (:file "export"             :depends-on (:environment))
   #+lispworks(:file "gui"     :depends-on ("export"))
   #|
   (:file "print-tree"        :depends-on ("export"))
   (:file "pokusy"            :depends-on ("export"))
   |#
   )
;  :properties ((:author-email . "jakubkalab@gmail.com")
;               (:date . "4.9.2010")
  )
