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
    (unwatch-all env)
    (assert-false (eenv::watched-p env :facts))
    (set-watcher env :facts)
    (assert-true (eenv::watched-p env :facts))
    (unset-watcher env :facts)
    (assert-false (eenv::watched-p env :facts))
    (watch-all env)
    (assert-true (and (eenv::watched-p env :facts)
                      (eenv::watched-p env :activations)))
    (unwatch-all env)
    (assert-false (and (eenv::watched-p env :facts)
                      (eenv::watched-p env :activations)))))

(def-test-method test-templates ((tests env-tests) :run nil)
  (with-slots (env tmpl) tests
    (assert-false (find-template env :tmpl))
    (add-template env tmpl)
    (assert-true (find-template env :tmpl))
    (assert-equal (find-template env :tmpl) tmpl)))

(def-test-method test-simple-facts ((tests env-tests) :run nil)
  (with-slots (env) tests
    (let ((fact (make-simple-fact '(in box hall))))
      (assert-false (find-fact env fact))
      (add-fact env fact)
      (assert-true (find-fact env fact))
      ;; fact equality is tested based on contents, so this should work:
      (rem-fact env (make-simple-fact '(in box hall)))
      (assert-false (find-fact env fact))
;      ;; fact is not there
;      (assert-condition 'simple-error (modify-fact env fact ()))
;      (add-fact env fact)
;      ;; modify-fact doesn't work for simple facts
;      (assert-condition 'simple-error (modify-fact env fact ()))
      )))

(def-test-method test-tmpl-facts ((tests env-tests) :run nil)
  (with-slots (env tmpl) tests
    (let ((fact (make-template-fact tmpl '(:a 3))))
      (assert-false (find-fact env fact))
      (add-fact env fact)
      (assert-true (find-fact env fact))
      ;; fact equality is tested based on contents, so this should work:
      (rem-fact env (make-template-fact tmpl '(:a 3)))
      (assert-false (find-fact env fact))
;      ;; fact is not there
;      (assert-condition 'simple-error (modify-fact env fact ()))
;      (add-fact env fact)
;      (modify-fact env fact '(:a 5 :b 10))
;      (assert-false (find-fact env fact))
;      (assert-true (find-fact env (make-template-fact tmpl '(:a 5 :b 10))))
      )))


(add-test-suite 'env-tests)
;(textui-test-run (get-suite env-tests))
