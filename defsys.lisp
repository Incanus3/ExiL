(in-package :cl-user)

(defsystem :exil
  ()
  :members ("packages" "utils"
                       "templates" "facts" "patterns" "rules"
		       "tokens" "rete-generic-node" "rete-alpha-part"
		       "rete-beta-part" "rete-net-creation"
                       "matches" "activations" "strategies" "environment"
                       "export" "test-package")
  :rules
   ((:load :all (:requires (:load :previous)))
    (:compile :all (:requires (:load :previous)))))

;(compile-system :exil :load t)
(load-system :exil)
