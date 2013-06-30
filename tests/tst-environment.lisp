(in-package :env-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass env-tests (test-case)
  ((env :accessor env)
   (tmpl :accessor tmpl :initform (make-template :tmpl '(a (b :default 5))))))

(defmethod set-up ((tests env-tests))
  (with-slots (env) tests
    (setf env (make-environment))))

(def-test-method test-watchers ((tests env-tests) :run nil)
  (with-slots (env) tests
    (unset-watcher env :all)
    (assert-false (eenv::watched-p env :facts))
    (set-watcher env :facts)
    (assert-true (eenv::watched-p env :facts))
    (unset-watcher env :facts)
    (assert-false (eenv::watched-p env :facts))
    (set-watcher env :all)
    (assert-true (and (eenv::watched-p env :facts)
                      (eenv::watched-p env :activations)))
    (unset-watcher env :all)
    (assert-false (and (eenv::watched-p env :facts)
		       (eenv::watched-p env :activations)))))

(def-test-method test-templates ((tests env-tests) :run nil)
  (with-slots (env tmpl) tests
    (assert-false (find-template env :tmpl))
    (add-template env tmpl)
    (assert-equal (find-template env :tmpl) tmpl)))

(def-test-method test-simple-facts ((tests env-tests) :run nil)
  (with-slots (env) tests
    (let ((fact (make-simple-fact '(in box hall))))
      (assert-false (find-fact env fact))
      (add-fact env fact)
      (assert-true (find-fact env fact))
      ;; fact equality is tested based on contents, so this should work:
      (rem-fact env (make-simple-fact '(in box hall)))
      (assert-false (find-fact env fact)))))

(def-test-method test-tmpl-facts ((tests env-tests) :run nil)
  (with-slots (env tmpl) tests
    (let ((fact (make-template-fact tmpl '(:a 3))))
      (assert-false (find-fact env fact))
      (add-fact env fact)
      (assert-true (find-fact env fact))
      ;; fact equality is tested based on contents, so this should work:
      (rem-fact env (make-template-fact tmpl '(:a 3)))
      (assert-false (find-fact env fact)))))

(def-test-method test-fact-groups ((tests env-tests) :run nil)
  (with-slots (env) tests
    (let ((fact1 (make-simple-fact '(in box hall)))
          (fact2 (make-simple-fact '(in robot warehouse))))
      (add-fact-group env :facts (list fact1 fact2))
      (assert-true (find-fact-group env :facts))
      ;; should add facts from the group
      (reset-env env)
      (assert-true (find-fact env fact1))
      (rem-fact-group env :facts)
      (assert-false (find-fact-group env :facts))
      (reset-env env)
      (assert-false (find-fact env fact1)))))

;; adding and changing strategies is an experimental feature, which probably
;; won't be used much; skipping tests for now

;; not much to test in activations functionality, if we take proper funcionality
;; of ext-pushnew, ext-delete (which has been tested) for granted; better
;; tested by examples

(def-test-method test-rules ((tests env-tests) :run nil)
  (with-slots (env) tests
    (let* ((condition (make-simple-pattern '(in ?obj ?loc)))
           (rule (make-rule :rule (list condition) ()))
           (token (erete::make-empty-token))
           (match (eenv::make-match rule token)))
      (add-rule env rule)
      (assert-true (find-rule env :rule))
      (add-match env rule (erete::make-empty-token))
      ;; should remove matches with rule
      (rem-rule env :rule)
      (assert-false (find-rule env :rule))
      (assert-false (find match (activations env))))))

;; environment cleanup functions are really simple, so they shouldn't need to be
;; tested

(add-test-suite 'env-tests)
;(textui-test-run (get-suite env-tests))
