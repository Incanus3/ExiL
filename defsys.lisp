(in-package :cl-user)

(defsystem :exil
  ()
  :members ("packages" "exil")
  :rules
   ((:load ("exil") (:requires (:load "packages")))
    (:compile :all (:requires (:load :previous)))))