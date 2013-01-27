(in-package :core-tests)
#|
(defclass simple-fact-tests (test-case)
  ((fact :accessor fact)))

(defmethod set-up ((tests simple-fact-tests))
  (setf (fact tests) (make-simple-fact '(in box hall))))

;;template-fact tests
(defclass template-fact-tests (test-case)
  ((fact :accessor fact)))

(defmethod set-up ((tests template-fact-tests))
  (setf (fact tests)
        (make-instance 'template-fact
                       :tmpl-name 'in
                       :slots (copy-alist '((object . box) (location . hall))))))

(add-test-suite 'simple-fact-tests)
(add-test-suite 'template-fact-tests)
;(textui-test-run (get-suite simple-fact-tests))
;(textui-test-run (get-suite template-fact-tests))
|#
