(in-package :tests-base)

(defvar *test-suites* ())
(defun add-test-suite (suite-name)
  (push-end suite-name *test-suites*))
(defun run-tests ()
  (let ((fail-count 0) (err-count 0) (total-count 0))
    (dolist (suite *test-suites*)
      (format t "~%Running test suite ~a " suite)
      (let ((results (textui-test-run (suite (make-instance suite)))))
        (incf total-count (xlunit::run-tests results))
	(incf fail-count (xlunit::failure-count results))
	(incf err-count (xlunit::error-count results))))
    (format t "~%------------------------------------------------------------~%")
    (format t "TOTAL: ~A~%ERRORS: ~A~%FAILURES: ~A" total-count err-count fail-count)
    (format t "~%------------------------------------------------------------")))

#|
(defmacro with-function-patch (patch &rest body)
  "Takes a PATCH form like a FLET clause, i.e. (fn-name (lambda-list) body),
   evaluates BODY in an environment with fn-name rebound to the PATCH form and
   uses UNWIND-PROTECT to safely restore the original definition afterwards."
  (let ((oldfn (gensym))
        (result (gensym))
        (name (car patch))
        (args (cadr patch))
        (pbody (cddr patch)))
    `(let ((,oldfn (symbol-function ',name)))
       (setf (symbol-function ',name) (lambda ,args ,@pbody))
       (unwind-protect (progn ,@body)
         (setf (symbol-function ',name) ,oldfn))
       ,result)))
|#
