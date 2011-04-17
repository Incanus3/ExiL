(require :asdf)
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
     (:file "rete-generic-node" :depends-on ("rules"))
     (:file "rete-alpha-part"   :depends-on ("rete-generic-node"))
     (:file "rete-beta-part"    :depends-on ("rete-alpha-part"))
     (:file "rete-net-creation" :depends-on ("rete-beta-part"))
     (:file "matches"           :depends-on ("rete-net-creation"))
     (:file "activations"       :depends-on ("matches"))
     (:file "strategies"        :depends-on ("activations"))
     (:file "environment"       :depends-on ("strategies"))
     (:file "print-tree"        :depends-on ("environment"))
     (:file "export"            :depends-on ("environment"))
;   (:file "pokusy"            :depends-on ("export"))
     )
    :properties ((:author-email . "jakubkalab@gmail.com")
		 (:date . "4.9.2010")
		 ((:albert :output-dir) . "doc")
		 ((:albert :formats) . ("docbook"))
		 ((:albert :docbook :template) . "book")
		 ((:albert :docbook :bgcolor) . "white")
		 ((:albert :docbook :textcolor) . "black")))
