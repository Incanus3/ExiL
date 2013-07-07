(in-package :exil-rete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests holds info about one variable-binding consistency test
;; current- and previous-field-to-test are object-slot descriptors -
;; integer positions for simple-facts, slot names for template facts

(defclass test ()
  ((current-field-to-test
    :reader current-field :initarg :current-field
    :initform (error "current-field slot has to be specified"))
   (previous-condition-number
    :documentation "tells, how many conditions back i must go"
    :reader previous-condition :initarg :previous-condition
    :initform 0)
   (previous-field-to-test
    :reader previous-field :initarg :previous-field
    :initform (error "previous-field slot has to be specified"))))

(defgeneric test-equal-p (test1 test2))
(defgeneric tests-equal-p (test-list1 test-list2))

(defun make-test (current-field previous-condition previous-field)
  (make-instance 'test
                 :current-field current-field
                 :previous-condition previous-condition
                 :previous-field previous-field))

(defmethod test-equal-p ((test1 test) (test2 test))
  (with-accessors ((cf1 current-field) (pc1 previous-condition)
                   (pf1 previous-field)) test1
    (with-accessors ((cf2 current-field) (pc2 previous-condition)
                     (pf2 previous-field)) test2
      (and (equalp cf1 cf2) (= pc1 pc2) (equalp pf1 pf2)))))

(defmethod tests-equal-p ((test-list1 list) (test-list2 list))
  (and (= (length test-list1) (length test-list2))
       (every #'test-equal-p test-list1 test-list2)))
