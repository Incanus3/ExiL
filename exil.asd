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
   (:file "global" :depends-on ("packages" "utils"))
   (:file "facts"  :depends-on ("global"))
   (:file "rules"  :depends-on ("global" "facts"))
   (:file "rete"   :depends-on ("global" "facts" "rules"))))