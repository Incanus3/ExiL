(in-package :cl-user)

(defsystem :exil
  ()
  :members ("packages" "utils" "exil")
  :rules
   ((:load :all (:requires (:load :previous)))
    (:compile :all (:requires (:load :previous)))))