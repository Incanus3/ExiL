(in-package :core-tests)

(defclass templates-tests (test-case)
  ((template :initform (make-template 'test-template '(a (b :default 5)))
             :reader template)
   (object :accessor object)))

(defmethod set-up ((tests templates-tests))
  (setf (object tests) (make-instance 'exil-core::template-object
                             :tmpl-name 'test-template
                             :slots (copy-alist '((a . 1) (b . 2))))))

(def-test-method test-variable-p ((tests templates-tests) :run nil)
  (assert-true (variable-p '?a))
  (assert-true (not (variable-p 'a))))

(def-test-method test-doslots ((tests templates-tests) :run nil)
  (let (slots)
    (doslots (name default (template tests))
      (push (cons name default) slots))
    (assert-equal (nreverse slots) '((a . nil) (b . 5)))))

(def-test-method test-has-slot-p ((tests templates-tests) :run nil)
  (with-slots (object) tests
    (assert-true (has-slot-p object 'a))
    (assert-true (not (has-slot-p object 'no-slot)))))

(def-test-method test-tmpl-object-slot-value ((tests templates-tests) :run nil)
  (with-slots (object) tests
    (assert-equal (exil-core::tmpl-object-slot-value object 'a) 1)
    (setf (exil-core::tmpl-object-slot-value object 'a) 5)
    (assert-equal (exil-core::tmpl-object-slot-value object 'a) 5)))

(def-test-method test-tmpl-object-equal-p ((tests templates-tests) :run nil)
  (with-slots (object) tests
    (assert-true (exil-core::tmpl-object-equal-p object object))
    (let ((object2 (make-instance 'exil-core::template-object
                                  :tmpl-name 'other-template
                                  :slots (slots object)))
          (object3 (make-instance 'exil-core::template-object
                                  :tmpl-name (tmpl-name object)
                                  :slots '((a . 1) (b . 3)))))
      (assert-true (not (exil-core::tmpl-object-equal-p object object2)))
      (assert-true (not (exil-core::tmpl-object-equal-p object object3))))))

(def-test-method test-find-atom ((tests templates-tests) :run nil)
  (with-slots (object) tests
    (assert-true (find-atom object 1))
    (assert-true (not (find-atom object 3)))))

(add-test-suite 'templates-tests)
;(textui-test-run (get-suite templates-tests))
