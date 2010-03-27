(in-package :cl-user)

(defsystem :exil
  ()
  :members ("packages" "utils" "facts" "patterns" "rules" "rete")
  :rules
   ((:load :all (:requires (:load :previous)))
    (:compile :all (:requires (:load :previous)))))
