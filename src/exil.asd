(require :asdf)
(defpackage :exil-system
  (:documentation "contains the ExiL ASDF system")
  (:use :asdf :cl))
(in-package :exil-system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsystem exil
  :name "EXpert system In Lisp"
  :author "Jakub Kalab <jakubkalab@gmail.com>"
  ;; :version "0.1"
  :maintainer "Jakub Kalab <jakubkalab@gmail.com>"
  ;; :licence "BSD"
  :description "EXpert system In Lisp"
  ;; :long-description ""
  :depends-on (:xlunit :iterate)
  :serial t
  :components
  ((:file "packages")
   (:module :utils
            :components
            ((:file "symbols") (:file "alists") (:file "plists")
             (:file "lists") (:file "hash-tables") (:file "misc")))
   (:module :core
            :components
            ((:file "templates") (:file "base-objects") (:file "facts")
	     (:file "patterns") (:file "pattern-matching") (:file "rules")))
   (:module :rete
            :components
            ((:file "tokens")
	     (:module
	      :rete-nodes
	      :components
	      ((:file "generic-node") (:file "alpha-top")
	       (:file "alpha-tests-mems") (:file "beta-tests")
	       (:file "beta-memories") (:file "beta-joins")))
	     (:file "rete-class") (:file "create-alpha-net")
             (:file "create-beta-net") (:file "rete-print")
	     (:file "graph-traversal") (:file "rete-equal")
	     (:file "rete-copy")))
   (:module :environment
            :components
            ((:file "matches") (:file "activate-rule") (:file "strategies")
             (:file "env-class") (:file "env-slots")
             (:file "env-copy") (:file "env-undo")
             (:file "env-watchers") (:file "env-templates") (:file "env-facts")
             (:file "env-strategies") (:file "env-agenda") (:file "env-rules")
             (:file "env-cleanup") (:file "env-inference")
             (:file "env-backward-base") (:file "env-backward-matching")
             (:file "env-backward-inference")))
   (:module :parser
            :components
            ((:file "prs-base") (:file "prs-templates") (:file "prs-facts")
             (:file "prs-rules")))
   (:module :front-end
            :components
            ((:file "fe-base") (:file "fe-facts") (:file "fe-rules")
             (:file "fe-execution")))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   #-lispworks(:module :tests
               :components
               ((:file "tst-base") (:file "tst-utils")
                (:module
                 :core
                 :components
                 ((:file "tst-templates") (:file "tst-base-objects")
                  (:file "tst-patterns")))
                (:module
                 :rete
                 :components
                 ((:file "tst-tokens") (:file "tst-rete")
                  (:file "tst-rete-walk") (:file "tst-rete-copy-simple")
                  (:file "tst-rete-copy-template") (:file "tst-rete-bugs")))
                (:module
                 :environment
                 :components
                 ((:file "tst-env-slots")
                  (:file "tst-env-simple") (:file "tst-env-template")
                  (:file "tst-env-backward")
                  (:file "tst-env-copy-simple") (:file "tst-env-copy-template")
                  (:file "tst-env-undo") (:file "tst-env-undo2")))
                (:module
                 :integration
                 :components
                 ((:file "simple") (:file "template") (:file "clips")
                  (:file "undo-redo") (:file "backward")
                  (:file "undo-redo-backward") (:file "functional")
                  (:file "rete-bugs")))
                (:file "run-tests")))
   #-lispworks(:module :examples
               :components
               ((:file "examples-template")
                (:file "examples-simple")
                (:file "examples-clips")))
   #+lispworks(:file "gui")
   ))
