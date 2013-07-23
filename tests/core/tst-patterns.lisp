(in-package :core-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass pattern-tests (test-case) ())

(def-test-method test-exil-equal-p ((tests pattern-tests) :run nil)
  (let* ((specifier '(on ?a ?b))
         (simple-pattern1 (make-simple-pattern specifier))
         (simple-pattern2 (make-simple-pattern '(on ?a ?c)))
         (simple-pattern3 (make-simple-pattern specifier :negated t))
         (tmpl1 (make-template :tmpl '(:a :b)))
         (tmpl2 (make-template :tmpl2 '(:a (:b :default 5))))
         (slots '(:a 1 :b 2))
         (tmpl-pattern1 (make-template-pattern tmpl1 slots))
         (tmpl-pattern2 (make-template-pattern tmpl1 ()))
         (tmpl-pattern3 (make-template-pattern tmpl2 slots))
         (tmpl-pattern4 (make-template-pattern tmpl1 slots :negated t)))
    (assert-true (exil-equal-p simple-pattern1 simple-pattern1))
    (assert-false (exil-equal-p simple-pattern1 simple-pattern2))
    (assert-false (exil-equal-p simple-pattern1 simple-pattern3))
    (assert-true (exil-equal-p tmpl-pattern1 tmpl-pattern1))
    (assert-false (exil-equal-p tmpl-pattern1 tmpl-pattern2))
    (assert-false (exil-equal-p tmpl-pattern1 tmpl-pattern3))
    (assert-false (exil-equal-p tmpl-pattern1 tmpl-pattern4))))

(def-test-method test-variable-p ((tests pattern-tests) :run nil)
  (assert-true (variable-p '?a))
  (assert-false (variable-p 'a)))

(def-test-method test-constant-test ((tests pattern-tests) :run nil)
  (assert-true (constant-test 'a 'a))
  (assert-true (constant-test '?a 'b))
  (assert-false (constant-test 'a 'b)))

(def-test-method test-var-or-equal-p ((tests pattern-tests) :run nil)
  (assert-true (var-or-equal-p '?a '?b))
  (assert-true (var-or-equal-p 'a 'a))
  (assert-false (var-or-equal-p 'a 'b))
  (assert-false (var-or-equal-p '?a 'a)))

(def-test-method test-match-against-pattern ((tests pattern-tests)
						  :run nil)
  (assert-equal
   (multiple-value-list
    (match-against-pattern (make-simple-fact '(in box hall))
				(make-simple-pattern '(in ?obj ?loc))))
   '(t ((?obj . box) (?loc . hall))))
  (assert-false (match-against-pattern (make-simple-fact '(in box hall))
					    (make-simple-pattern '(in box garden))))
  (assert-false (match-against-pattern (make-simple-fact '(in box hall))
					    (make-simple-pattern '(in ?a ?a))))
  (let ((tmpl (make-template :in '(obj loc))))
    (assert-equal
     (multiple-value-list
      (match-against-pattern
       (make-template-fact tmpl '(:obj box :loc hall))
       (make-template-pattern tmpl '(:obj ?obj :loc ?loc))))
     '(t ((?obj . box) (?loc . hall))))
    (assert-false (match-against-pattern
		   (make-template-fact tmpl '(:obj box :loc hall))
		   (make-template-pattern tmpl '(:obj box :loc garden))))
    (assert-false (match-against-pattern
		   (make-template-fact tmpl '(:obj box :loc hall))
		   (make-template-pattern tmpl '(:obj ?a :loc ?a))))))

(add-test-suite 'pattern-tests)
;(textui-test-run (get-suite pattern-tests))
