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
   (:file "utils"              :depends-on ("packages"))             ; ^
   (:file "tests"              :depends-on ("utils"))                ; | utils
   (:file "utils-tests"        :depends-on ("utils" "tests"))        ; v
   (:file "templates"          :depends-on ("utils"))                ; ^
   (:file "templates-tests"    :depends-on ("templates" "tests"))    ; |
   (:file "base-objects"       :depends-on ("templates"))            ; |
   (:file "base-objects-tests" :depends-on ("base-objects" "tests")) ; |
   (:file "facts"              :depends-on ("base-objects"))         ; |
   (:file "facts-tests"        :depends-on ("facts" "tests"))        ; | core
   (:file "patterns"           :depends-on ("facts"))                ; |
   (:file "patterns-tests"     :depends-on ("patterns" "tests"))     ; |
   (:file "rules"              :depends-on ("patterns"))             ; |
   (:file "rules-tests"        :depends-on ("rules" "tests"))        ; v
   (:file "tokens"             :depends-on ("rules"))                ; ^
   (:file "tokens-tests"       :depends-on ("tokens" "tests"))       ; |
   (:file "rete-generic-node"  :depends-on ("tokens"))               ; |
   (:file "rete-alpha-part"    :depends-on ("rete-generic-node"))    ; | rete
   (:file "rete-beta-part"     :depends-on ("rete-alpha-part"))      ; |
   (:file "rete-net-creation"  :depends-on ("rete-beta-part"))       ; v
   (:file "matches"            :depends-on ("rete-net-creation"))    ; ^
   (:file "activations"        :depends-on ("matches"))              ; |
   (:file "strategies"         :depends-on ("activations"))          ; | environment
   (:file "environment"        :depends-on ("strategies"))           ; |
   (:file "object-makers"      :depends-on ("environment"))          ; v
   (:file "export"             :depends-on ("object-makers"))        ; front-end
;   (:file "gui"                :depends-on ("export"))
;   (:file "test-package"       :depends-on ("export"))
   #|     (:file "print-tree"        :depends-on ("environment"))
     (:file "pokusy"            :depends-on ("export"))|#)
;  :properties ((:author-email . "jakubkalab@gmail.com")
;               (:date . "4.9.2010")
  )
