(in-package :env-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass template-env-copy-tests (template-env-tests) ())

(def-test-method test-copy-rete ((tests template-env-copy-tests) :run nil)
  (with-slots (env t-in) tests
    (do-step env)
    (let ((env-copy (eenv::copy-environment env)))
      (setf *env* env-copy)
      (do-step env-copy)
      (do-step env-copy)
      (assert-true (find-fact env-copy
			      (make-template-fact t-in '(:object box :location A)))))))

(add-test-suite 'template-env-copy-tests)
;(textui-test-run (get-suite template-env-copy-tests))
