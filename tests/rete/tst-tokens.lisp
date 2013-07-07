(in-package :rete-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass token-tests (test-case)
  ((wme1 :initform (make-simple-fact '(in box hall)) :reader wme1)
   (wme2 :initform (make-simple-fact '(in robot warehouse)) :reader wme2)
   (empty-token :initform (erete::make-empty-token) :reader empty-token)
   (empty-token2 :initform (erete::make-empty-token) :reader empty-token2)
   (token1 :accessor token1)
   (token2 :accessor token2)
   (token3 :accessor token3)
   (token4 :accessor token4)))

(defmethod set-up ((tests token-tests))
  (with-slots (token1 token2 token3 token4 wme1 wme2) tests
    (setf token1 (erete::make-token wme1 (erete::make-token wme2))
          token2 (erete::make-token wme1 (erete::make-token wme2))
          token3 (erete::make-token wme1)
          token4 (erete::make-token wme2))))

(def-test-method test-token-equal-p ((tests token-tests) :run nil)
  (with-slots (token1 token2 token3 empty-token empty-token2) tests
    (assert-true (token-equal-p token1 token2))
    (assert-false (token-equal-p token1 token3))
    (assert-true (token-equal-p empty-token empty-token2))
    (assert-false (token-equal-p token1 empty-token))
    (assert-false (token-equal-p empty-token token1))))

(def-test-method test-previous-wme ((tests token-tests) :run nil)
  (with-slots (token1 wme1 wme2) tests
    (assert-equal (erete::previous-wme token1 0) wme1)
    (assert-equal (erete::previous-wme token1 1) wme2)
    (assert-false (erete::previous-wme token1 2))))

(def-test-method test-included-in-p-fact ((tests token-tests) :run nil)
  (with-slots (token1 token3 empty-token wme1 wme2) tests
    (assert-true (erete::included-in-p wme1 token1))
    (assert-true (erete::included-in-p wme2 token1))
    (assert-false (erete::included-in-p wme2 token3))
    (assert-false (erete::included-in-p wme1 empty-token))))

(def-test-method test-included-in-p-token ((tests token-tests) :run nil)
  (with-slots (token1 token2 token3 token4 empty-token) tests
    (assert-true (erete::included-in-p token2 token1)) ;; they're exil-equal-p
    ;; (parent token1) is exil-equal-p to token4
    (assert-true (erete::included-in-p token4 token1))
    (assert-false (erete::included-in-p token3 token1))))

(def-test-method test-token->list ((tests token-tests) :run nil)
  (with-slots (token1 empty-token wme1 wme2) tests
    (assert-equal (token->list token1) (list wme2 wme1))
    (assert-false (token->list empty-token))))

(add-test-suite 'token-tests)
;(textui-test-run (get-suite token-tests))
