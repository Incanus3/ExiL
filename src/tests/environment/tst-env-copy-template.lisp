(in-package :env-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass template-env-copy-tests (template-env-tests) ())

(def-test-method copy-can-continue-computation
    ((tests template-env-copy-tests) :run nil)
  (with-slots (env t-in) tests
    (step-env env)
    (let ((env-copy (copy-env env)))
      (setf *env* env-copy)
      (step-env env-copy)
      (step-env env-copy)
      (assert-true
       (find-fact env-copy
		  (make-template-fact t-in '(:object box :location A)))))))

(add-test-suite 'template-env-copy-tests)
;(textui-test-run (get-suite template-env-copy-tests))
