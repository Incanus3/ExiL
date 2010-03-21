(in-package :cl-user)

(defsystem :exil
  ()
  :members ("packages" "utils" "global" "facts" "rules" "rete")
  :rules
   ((:load :all (:requires (:load :previous)))
    (:compile :all (:requires (:load :previous)))))