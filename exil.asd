(defpackage :exil-system (:use :asdf :cl))
(in-package :exil-system)

(defsystem exil
  :name "EXpert system In Lisp"
  :author "Jakub Kalab <jakubkalab@gmail.com>"
  :version "0.1"
  :maintainer "Jakub Kalab <jakubkalab@gmail.com>"
  :licence "BSD"
  :description "EXpert system In Lisp"
  :long-description ""
  :components
  ((:file "packages")
   (:file "utils"             :depends-on ("packages"))
   (:file "templates"         :depends-on ("utils"))
   (:file "facts"             :depends-on ("templates"))
   (:file "patterns"          :depends-on ("facts"))
   (:file "rules"             :depends-on ("patterns"))
   (:file "rete-activations"  :depends-on ("rules"))
   (:file "rete-net-creation" :depends-on ("rete-activations"))
   (:file "print-tree"        :depends-on ("rete-activations"))
   (:file "environment"       :depends-on ("rete-net-creation"))
   (:file "export"            :depends-on ("environment"))))

