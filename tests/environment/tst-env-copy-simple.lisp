(in-package :env-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass simple-env-copy-tests (simple-env-tests) ())

(def-test-method test-copy-rete ((tests simple-env-copy-tests) :run nil)
  (with-slots (env) tests
    (do-step env)
    (let ((env-copy (eenv::copy-environment env)))
      (setf *env* env-copy)
      (do-step env-copy)
      (do-step env-copy)
      (assert-true (find-fact env-copy (make-simple-fact '(in box A)))))))

(add-test-suite 'simple-env-copy-tests)
;(textui-test-run (get-suite simple-env-copy-tests))
