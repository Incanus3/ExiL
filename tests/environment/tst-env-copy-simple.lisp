(in-package :env-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass simple-env-copy-tests (simple-env-tests) ())

(def-test-method copy-has-no-common-slots
    ((tests simple-env-copy-tests) :run nil)
  (with-slots (env) tests
    ;; all slots should be populated
    (add-fact-group env :facts ())
    (set-watcher env :facts)
    (undo env)
    (let ((env-copy (copy-env env)))
      (assert-false (common-slots-p env env-copy)))))

(def-test-method copy-can-continue-computation
    ((tests simple-env-copy-tests) :run nil)
  (with-slots (env) tests
    (do-step env)
    (let ((env-copy (copy-env env)))
      (setf *env* env-copy)
      (do-step env-copy)
      (do-step env-copy)
      (assert-true (find-fact env-copy (make-simple-fact '(in box A)))))))

(add-test-suite 'simple-env-copy-tests)
;(textui-test-run (get-suite simple-env-copy-tests))
