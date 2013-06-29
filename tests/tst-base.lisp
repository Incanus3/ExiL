(in-package :tests-base)

(defvar *test-suites* ())
(defun add-test-suite (suite-name)
  (push-end suite-name *test-suites*))
(defun run-suites ()
  (dolist (suite *test-suites*)
    (format t "~%Running test suite ~a " suite)
    (textui-test-run (suite (make-instance suite)))))

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
