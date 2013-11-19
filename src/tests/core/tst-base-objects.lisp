(in-package :core-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass simple-object-tests (test-case)
  ((object :accessor object)
   (object2 :reader object2
            :initform (make-instance 'exil-core::simple-object
                                     :specifier '(in robot warehouse)))))

(defmethod set-up ((tests simple-object-tests))
  (setf (object tests) (make-instance 'exil-core::simple-object
                                      :specifier (copy-list '(in box hall)))))

(def-test-method test-exil-equal-p ((tests simple-object-tests) :run nil)
  (with-slots (object object2) tests
    (assert-true (exil-equal-p object object))
    (assert-false (exil-equal-p object object2))))

(def-test-method test-object-slot ((tests simple-object-tests) :run nil)
  (with-slots (object) tests
    (assert-equal (object-slot object 1) 'box)
    (setf (object-slot object 1) 'blue-box)
    (assert-equal (object-slot object 1) 'blue-box)))

#|
(def-test-method test-find-atom ((tests simple-object-tests) :run nil)
  (with-slots (object) tests
    (assert-true (find-atom object 'box))
    (assert-false (find-atom object 'not-present))))
|#

(def-test-method test-atom-position ((tests simple-object-tests) :run nil)
  (with-slots (object) tests
    (assert-equal (atom-position object 'box) 1)
    (assert-false (atom-position object 'not-present))))

(def-test-method test-description ((tests simple-object-tests) :run nil)
  (with-slots (object) tests
    (assert-equal (description object) '(in box hall))))

(defclass template-object-tests (test-case)
  ((template :initform (make-template :test-template '(a (b :default 5)))
             :reader template)
   (template2 :initform (make-template :test-template2 '(a (b :default 10)))
             :reader template2)
   (object :accessor object)
   (object2 :accessor object2)
   (object3 :accessor object3)))

(defmethod set-up ((tests template-object-tests))
  (with-slots (template template2 object object2 object3) tests
    (setf object
          (exil-core::make-tmpl-object template '(:a 1 :b 2)
                                       'exil-core::template-object)
          object2
          ;; different slots
          (exil-core::make-tmpl-object template '(:a 2 :b 3)
                                       'exil-core::template-object)
          object3
          ;; same slots, different template
          (exil-core::make-tmpl-object template2 '(:a 1 :b 2)
                                       'exil-core::template-object))))

(def-test-method test-exil-equal-p ((tests template-object-tests) :run nil)
  (with-slots (object object2 object3) tests
    (assert-true (exil-equal-p object object))
    (assert-false (exil-equal-p object object2))
    (assert-false (exil-equal-p object object3))))

#|
(def-test-method test-has-slot-p ((tests template-object-tests) :run nil)
  (with-slots (object) tests
    (assert-true (has-slot-p object 'a))
    (assert-false (has-slot-p object 'no-slot))))
|#

(def-test-method test-object-slot ((tests template-object-tests) :run nil)
  (with-slots (object) tests
    (assert-equal (object-slot object :a) 1)
    (setf (object-slot object :a) 5)
    (assert-equal (object-slot object :a) '5)))

#|
(def-test-method test-find-atom ((tests template-object-tests) :run nil)
  (with-slots (object) tests
    (assert-true (find-atom object 1))
    (assert-false (find-atom object 3))))
|#

(def-test-method test-atom-position ((tests template-object-tests) :run nil)
  (with-slots (object) tests
    (assert-equal (atom-position object 2) :b)
    (assert-false (atom-position object 3))))

(def-test-method test-description ((tests template-object-tests) :run nil)
  (with-slots (object) tests
    (assert-equal (description object) '(:test-template :a 1 :b 2))))

(add-test-suite 'simple-object-tests)
(add-test-suite 'template-object-tests)
;(textui-test-run (get-suite simple-object-tests))
;(textui-test-run (get-suite template-object-tests))
