(in-package :rete-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass copy-rete-tests (test-case)
  ((rete :accessor rete)))

(defmethod set-up ((tests copy-rete-tests))
  (setf (rete tests) (make-rete (make-instance 'env-mock))))

(def-test-method test-copy-rete ((tests copy-rete-tests) :run nil)
  (with-slots (rete) tests
    (let* ((new-env (make-instance 'env-mock))
	   (new-rete (copy-rete rete new-env)))
      (assert-true (erete::rete-equal-p new-rete rete))
      ;; they shouldn't have any common nodes
      (assert-false (intersection (erete::linearize-graph new-rete)
				  (erete::linearize-graph rete))))))

;; test zarve na volani rete-equal-p, coz znamena, ze copy-rete projde,
;; coz je zvlastni - melo by se zaseknout u alpha-test-node, pro ktery
;; neni definovana copy-node

(add-test-suite 'copy-rete-tests)
;(textui-test-run (get-suite copy-rete-tests))
