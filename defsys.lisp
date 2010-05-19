(in-package :cl-user)

(defsystem :exil
  ()
  :members ("packages" "utils" "templates" "facts" "patterns"
		       "rules" "rete" "environment" "export")
  :rules
   ((:load :all (:requires (:load :previous)))
    (:compile :all (:requires (:load :previous)))))

(compile-system :exil :load t)