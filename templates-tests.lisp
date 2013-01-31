(in-package :core-tests)

(defclass template-tests (test-case)
  ((template :initform (make-template 'test-template '(a (b :default 5)))
             :reader template)))

(def-test-method test-doslots ((tests template-tests) :run nil)
  (let (slots)
    (doslots (name default (template tests))
      (push (cons name default) slots))
    (assert-equal (nreverse slots) '((a . nil) (b . 5)))))

(add-test-suite 'template-tests)
;(textui-test-run (get-suite template-tests))
