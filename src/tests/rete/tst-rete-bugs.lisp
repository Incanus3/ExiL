(in-package :rete-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass rete-bugs-tests (rete-tests) ())

(def-test-method test-find-atom-in-cond-list ((tests rete-bugs-tests) :run t)
  (let ((conds (list (make-simple-pattern '(in box ?loc))
                     (make-simple-pattern '(in robot ?loc2)))))
    (assert-equal (erete::find-atom-in-cond-list '?loc conds) (cons 2 2))))

(def-test-method rete-shouldnt-create-test-against-neg-conds ((tests rete-bugs-tests) :run t)
  (with-slots (env rete) tests
    (let* ((wme1 (make-simple-fact '(in obj1 A)))
           (wme2 (make-simple-fact '(in obj2 B)))
           (token1 (erete::make-token wme1))
           (token2 (erete::make-token wme2 token1))
           (rule (make-rule
                  :rule
                  (list (make-simple-pattern '(in obj1 ?loc1))
                        (make-simple-pattern '(next-to ?loc1 ?loc2) :negated t)
                        (make-simple-pattern '(in obj2 ?loc2)))
                  ())))
      (new-production rete rule)
      (add-wme rete wme1)
      (add-wme rete wme2)
      ;; the join node for 3rd condition shouldn't have any tests, as we can't
      ;; test against negative condition
      (assert-false (erete::tests
                     (third (erete::rete-nodes rete 'erete::beta-join-node))))
      ;; once this test is gone, the rule should be satisfied
      (assert-true (has-match env rule token2)))))

(add-test-suite 'rete-bugs-tests)
;;(textui-test-run (get-suite rete-bugs-tests))
