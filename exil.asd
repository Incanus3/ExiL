(defpackage :exil-system (:use :asdf :cl))
(in-package :exil-system)

(defsystem exil
  :name "EXpert system In Lisp"
  :author "Jakub Kaláb <jakubkalab@gmail.com>"
  :version "0.1"
  :maintainer "Jakub Kaláb <jakubkalab@gmail.com>"
  :licence "BSD"
  :description "EXpert system In Lisp"
  :long-description ""
  :components
  ((:file "packages")
   (:file "exil" :depends-on ("packages"))))
