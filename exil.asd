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
  :components
  ((:file "packages")
   (:file "tests")
   (:file "utils"             :depends-on ("packages"))  ; utils
   (:file "utils-tests"       :depends-on ("utils" "tests"))
   (:file "templates"         :depends-on ("utils"))     ; ^
   (:file "templates-tests"   :depends-on ("templates" "tests"))
   (:file "facts"             :depends-on ("templates")) ; |
   (:file "patterns"          :depends-on ("facts"))     ; | core
   (:file "rules"             :depends-on ("patterns"))  ; v
   (:file "tokens"            :depends-on ("rules"))             ; ^
   (:file "rete-generic-node" :depends-on ("tokens"))            ; |
   (:file "rete-alpha-part"   :depends-on ("rete-generic-node")) ; | rete
   (:file "rete-beta-part"    :depends-on ("rete-alpha-part"))   ; |
   (:file "rete-net-creation" :depends-on ("rete-beta-part"))    ; v
   (:file "matches"           :depends-on ("rete-net-creation")) ; ^
   (:file "activations"       :depends-on ("matches"))           ; |
   (:file "strategies"        :depends-on ("activations"))       ; | environment
   (:file "environment"       :depends-on ("strategies"))        ; |
   (:file "object-makers"     :depends-on ("environment"))       ; v
   (:file "export"            :depends-on ("object-makers"))     ; front-end
;   (:file "gui"               :depends-on ("export"))
;   (:file "test-package"      :depends-on ("export"))
   #|     (:file "print-tree"        :depends-on ("environment"))
     (:file "pokusy"            :depends-on ("export"))|#)
;  :properties ((:author-email . "jakubkalab@gmail.com")
;               (:date . "4.9.2010")
  )
