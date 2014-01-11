(in-package :rete-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass rete-bugs-tests (rete-tests) ())

(def-test-method test-find-atom-in-cond-list ((tests rete-bugs-tests) :run nil)
  (let ((conds (list (make-simple-pattern '(in box ?loc))
                     (make-simple-pattern '(in robot ?loc2)))))
    (assert-equal (erete::find-atom-in-cond-list '?loc conds) (cons 2 2))))

(def-test-method rete-shouldnt-create-test-against-neg-conds
    ((tests rete-bugs-tests) :run nil)
  (with-slots (env rete) tests
    (let* ((wme1 (make-simple-fact '(in obj1 A)))
           (wme2 (make-simple-fact '(in obj2 B)))
           (token1 (erete::make-token wme1))
           (token2 (erete::make-token nil token1))
           (token3 (erete::make-token wme2 token2))
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
      (let ((node (third (erete::rete-nodes rete 'erete::beta-join-node))))
        (assert-false (erete::tests node))
        ;; once this test is gone, the rule should be satisfied
        (assert-true (has-match env rule token3))))))

(def-test-method rete-should-create-correct-test-when-skipping-neg-conds
    ((tests rete-bugs-tests) :run nil)
  (with-slots (env rete) tests
    (let* ((wme1 (make-simple-fact '(goal box B)))
           (wme2 (make-simple-fact '(in box A)))
           (wme3 (make-simple-fact '(in robot A)))
           (token1 (erete::make-token wme1))
           (token2 (erete::make-token nil token1))
           (token3 (erete::make-token wme2 token2))
           (token4 (erete::make-token wme3 token3))
           (rule (make-rule
                  :push
                  (list (make-simple-pattern '(goal ?object ?goalloc))
                        (make-simple-pattern '(in ?object ?goalloc) :negated t)
                        (make-simple-pattern '(in ?object ?objloc))
                        (make-simple-pattern '(in robot ?objloc)))
                  ())))
      (new-production rete rule)
      (add-wme rete wme1)
      (add-wme rete wme2)
      (add-wme rete wme3)

      ;; this is the join node that joins 3rd condition against 1st
      ;; the test has '2 conds back', but the token doesn't have fact for neg cond
      ;; THE TOKEN SHOULD INCLUDE PLACEHOLDER FOR THE NEG COND
      ;; (let ((node (second (erete::rete-nodes rete 'erete::beta-join-node))))
      ;;   (print (erete::previous-condition (first (erete::tests node)))))

      ;; this is the memory node before him, that stores the token
      (let* ((node (fourth (erete::rete-nodes rete 'erete::beta-memory-node)))
             (token (first (erete::items node)))
             (token-list (token->list token)))
        ;; the token should be ((goal box b) nil)
        (assert-equal (length token-list) 2)
        (assert-equal (second token-list) nil))

      (assert-true (has-match env rule token4)))))

(add-test-suite 'rete-bugs-tests)
;;(textui-test-run (get-suite rete-bugs-tests))
