(in-package :core-tests)

(defclass facts-tests (test-case)
  ((template :initform (make-template 'test-template '(a (b :default 5)))
             :reader template)
   (simple-fact :initform (make-simple-fact '(in box hall)) :reader simple-fact)
   (template-fact :accessor template-fact)))

(defmethod set-up ((tests facts-tests))
  (setf (template-fact tests)
        (make-instance 'template-fact
                       :tmpl-name 'in
                       :slots '((object . box) (location . hall)))))

;; simple-fact tests
(def-test-method test-fact-equal-p ((tests facts-tests) :run nil)
  (with-slots (simple-fact) tests
    (let ((fact2 (make-simple-fact '(in robot warehouse))))
      (assert-true (fact-equal-p simple-fact simple-fact))
      (assert-false (fact-equal-p simple-fact fact2)))))

(def-test-method test-find-atom ((tests facts-tests) :run nil)
  (with-slots (simple-fact) tests
    (assert-true (find-atom simple-fact 'box))
    (assert-false (find-atom simple-fact 'not-present))))

(def-test-method test-atom-position ((tests facts-tests) :run nil)
  (with-slots (simple-fact) tests
    (assert-equal (atom-position simple-fact 'box) 1)
    (assert-false (atom-position simple-fact 'not-present))))

(def-test-method test-fact-description ((tests facts-tests) :run nil)
  (with-slots (simple-fact) tests
    (assert-equal (fact-description simple-fact) '(in box hall))))

(def-test-method test-copy-fact ((tests facts-tests) :run nil)
  (with-slots (simple-fact) tests
    (let ((fact-copy (copy-fact simple-fact)))
      (assert-true (fact-equal-p simple-fact fact-copy))
      (assert-not-eql simple-fact fact-copy))))

;;template-fact tests


(add-test-suite 'facts-tests)
(textui-test-run (get-suite facts-tests))
