(in-package :rete-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass token-tests (test-case)
  ((wme1 :initform (make-simple-fact '(in box hall)) :reader wme1)
   (wme2 :initform (make-simple-fact '(in robot warehouse)) :reader wme2)
   (empty-token :initform (erete::make-empty-token) :reader empty-token)
   (empty-token2 :initform (erete::make-empty-token) :reader empty-token2)
   (token1 :accessor token1)
   (token2 :accessor token2)
   (token3 :accessor token3)))

(defmethod set-up ((tests token-tests))
  (with-slots (token1 token2 token3 wme1 wme2) tests
    (setf token1 (erete::make-token wme1 (erete::make-token wme2))
          token2 (erete::make-token (copy-object wme1)
                                    (erete::make-token (copy-object wme2)))
          token3 (erete::make-token (copy-object wme1)))))

(def-test-method test-token-equal-p ((tests token-tests) :run nil)
  (with-slots (token1 token2 token3 empty-token empty-token2) tests
    (assert-true (token-equal-p token1 token2))
    (assert-false (token-equal-p token1 token3))
    (assert-true (token-equal-p empty-token empty-token2))
    (assert-false (token-equal-p token1 empty-token))
    (assert-false (token-equal-p empty-token token1))))

(add-test-suite 'token-tests)
;(textui-test-run (get-suite token-tests))
