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
   (:file "utils"  :depends-on ("packages"))
   (:file "global" :depends-on ("packages"))
   (:file "facts"  :depends-on ("packages" "utils"))
   (:file "rules"  :depends-on ("packages" "rules"))
   (:file "rete"   :depends-on ("packages" "facts" "rules"))))