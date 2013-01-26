(in-package :core-tests)

(defclass simple-fact-tests (test-case)
  ((fact :accessor fact)))

(defmethod set-up ((tests simple-fact-tests))
  (setf (fact tests) (make-simple-fact (list 'in 'box 'hall))))

;; simple-fact tests
(def-test-method test-fact-equal-p ((tests simple-fact-tests) :run nil)
  (with-slots (fact) tests
    (let ((fact2 (make-simple-fact '(in robot warehouse))))
      (assert-true (fact-equal-p fact fact))
      (assert-false (fact-equal-p fact fact2)))))

(def-test-method test-find-atom ((tests simple-fact-tests) :run nil)
  (with-slots (fact) tests
    (assert-true (find-atom fact 'box))
    (assert-false (find-atom fact 'not-present))))

(def-test-method test-atom-position ((tests simple-fact-tests) :run nil)
  (with-slots (fact) tests
    (assert-equal (atom-position fact 'box) 1)
    (assert-false (atom-position fact 'not-present))))

(def-test-method test-fact-description ((tests simple-fact-tests) :run nil)
  (with-slots (fact) tests
    (assert-equal (fact-description fact) '(in box hall))))

(def-test-method test-fact-slot ((tests simple-fact-tests) :run nil)
  (with-slots (fact) tests
    (assert-equal (fact-slot fact 1) 'box)
    (setf (fact-slot fact 1) 'blue-box)
    (assert-equal (fact-slot fact 1) 'blue-box)))

(def-test-method test-copy-fact ((tests simple-fact-tests) :run nil)
  (with-slots (fact) tests
    (let ((fact-copy (copy-fact fact)))
      (assert-true (fact-equal-p fact fact-copy))
      (assert-not-eql fact fact-copy))))

;;template-fact tests
(defclass template-fact-tests (test-case)
  ((fact :accessor fact)))

(defmethod set-up ((tests template-fact-tests))
  (setf (fact tests)
        (make-instance 'template-fact
                       :tmpl-name 'in
                       :slots (list (cons 'object 'box)
                                    (cons 'location 'hall)))))

(def-test-method test-fact-equal-p ((tests template-fact-tests) :run nil)
  (with-slots (fact) tests
    (let ((fact2 (copy-fact fact)))
      (assert-true (fact-equal-p fact fact2))
      (assert-not-eql fact fact2))))

(def-test-method test-fact-description ((tests template-fact-tests) :run nil)
  (with-slots (fact) tests
    (assert-equal (fact-description fact) '(in :object box :location hall))))

(def-test-method test-fact-slot ((tests template-fact-tests) :run nil)
  (with-slots (fact) tests
    (assert-equal (fact-slot fact 'object) 'box)
    (setf (fact-slot fact 'object) 'blue-box)
    (assert-equal (fact-slot fact 'object) 'blue-box)))

(def-test-method test-copy-fact ((tests template-fact-tests) :run nil)
  (with-slots (fact) tests
    (let ((fact-copy (copy-fact fact)))
      (assert-true (fact-equal-p fact fact-copy))
      (assert-not-eql fact fact-copy))))

(add-test-suite 'simple-fact-tests)
(add-test-suite 'template-fact-tests)
(textui-test-run (get-suite simple-fact-tests))
(textui-test-run (get-suite template-fact-tests))
