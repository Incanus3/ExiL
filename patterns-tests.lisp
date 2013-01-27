(in-package :core-tests)

(defclass pattern-tests (test-case) ())

(def-test-method test-exil-equal-p ((tests pattern-tests) :run nil)
  (let* ((specifier '(on ?a ?b))
         (simple-pattern1 (make-simple-pattern specifier))
         (simple-pattern2 (make-simple-pattern '(on ?a ?c)))
         (simple-pattern3 (make-simple-pattern specifier t)) ; negated
         (slots '((a . 1) (b . 2)))
         (tmpl-pattern1 (make-instance 'template-pattern
                                       :tmpl-name 'template
                                       :slots slots))
         (tmpl-pattern2 (make-instance 'template-pattern
                                       :tmpl-name 'template
                                       :slots ()))
         (tmpl-pattern3 (make-instance 'template-pattern
                                       :tmpl-name 'other-template
                                       :slots slots))
         (tmpl-pattern4 (make-instance 'template-pattern
                                       :tmpl-name 'template
                                       :slots slots
                                       :negated t)))
    (assert-true (exil-equal-p simple-pattern1 simple-pattern1))
    (assert-false (exil-equal-p simple-pattern1 simple-pattern2))
    (assert-false (exil-equal-p simple-pattern1 simple-pattern3))
    (assert-true (exil-equal-p tmpl-pattern1 tmpl-pattern1))
    (assert-false (exil-equal-p tmpl-pattern1 tmpl-pattern2))
    (assert-false (exil-equal-p tmpl-pattern1 tmpl-pattern3))
    (assert-false (exil-equal-p tmpl-pattern1 tmpl-pattern4))))

(def-test-method test-constant-test ((tests pattern-tests) :run nil)
  (assert-true (constant-test 'a 'a))
  (assert-true (constant-test '?a 'b))
  (assert-false (constant-test 'a 'b)))

(def-test-method test-var-or-equal-p ((tests pattern-tests) :run nil)
  (assert-true (var-or-equal-p '?a '?b))
  (assert-true (var-or-equal-p 'a 'a))
  (assert-false (var-or-equal-p 'a 'b))
  (assert-false (var-or-equal-p '?a 'a)))

(add-test-suite 'pattern-tests)
;(textui-test-run (get-suite pattern-tests))
