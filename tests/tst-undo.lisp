(in-package :undo-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass undo-tests (test-case)
  ((env :accessor env)))

(defmethod set-up ((tests undo-tests))
  (with-slots (env) tests
    (setf env (exil-env:make-environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WATCHERS

(def-test-method undo-watch-one ((tests undo-tests) :run nil)
  (with-slots (env) tests
    (unset-watcher env :facts) ; facts unwatched
    (set-watcher env :facts)   ; facts watched
    (undo env)                 ; facts should be unwatched again
    (assert-false (watched-p env :facts))))

(def-test-method undo-unwatch-one ((tests undo-tests) :run nil)
  (with-slots (env) tests
    (set-watcher env :facts)   ; facts watched
    (unset-watcher env :facts) ; facts unwatched
    (undo env)                 ; facts should be watched again
    (assert-true (watched-p env :facts))))

(def-test-method undo-watch-all ((tests undo-tests) :run nil)
  (with-slots (env) tests
    (unset-watcher env :facts) ; facts unwatched
    (unset-watcher env :rules) ; rules unwatched
    (set-watcher env :all)     ; all watched
    (undo env)                 ; facts and rules should be unwatched again
    (assert-false (watched-p env :facts))
    (assert-false (watched-p env :rules))))

(def-test-method undo-unwatch-all ((tests undo-tests) :run nil)
  (with-slots (env) tests
    (set-watcher env :facts)   ; facts watched
    (set-watcher env :rules)   ; rules watched
    (unset-watcher env :all)   ; all unwatched
    (undo env)                 ; facts and rules should be watched again
    (assert-true (watched-p env :facts))
    (assert-true (watched-p env :rules))))

(def-test-method redo-watch-one ((tests undo-tests) :run nil)
  (with-slots (env) tests
    (unset-watcher env :facts) ; facts unwatched
    (set-watcher env :facts)   ; facts watched
    (undo env)                 ; facts unwatched again
    (redo env)                 ; facts should be watched again
    (assert-true (watched-p env :facts))))

(def-test-method redo-unwatch-one ((tests undo-tests) :run nil)
  (with-slots (env) tests
    (set-watcher env :facts)   ; facts watched
    (unset-watcher env :facts) ; facts unwatched
    (undo env)                 ; facts watched again
    (redo env)                 ; facts should be unwatched again
    (assert-false (watched-p env :facts))))

(def-test-method redo-watch-all ((tests undo-tests) :run nil)
  (with-slots (env) tests
    (unset-watcher env :facts) ; facts unwatched
    (unset-watcher env :rules) ; rules unwatched
    (set-watcher env :all)     ; all watched
    (undo env)                 ; facts and rules unwatched again
    (redo env)                 ; facts and rules should be watched again
    (assert-true (watched-p env :facts))
    (assert-true (watched-p env :rules))))

(def-test-method redo-unwatch-all ((tests undo-tests) :run nil)
  (with-slots (env) tests
    (set-watcher env :facts)   ; facts watched
    (set-watcher env :rules)   ; rules watched
    (unset-watcher env :all)   ; all unwatched
    (undo env)                 ; facts and rules watched again
    (redo env)                 ; facts and rules should be unwatched again
    (assert-false (watched-p env :facts))
    (assert-false (watched-p env :rules))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEMPLATES

(def-test-method undo-add-template ((tests undo-tests) :run nil)
  (with-slots (env) tests
    (let ((tmpl1 (make-template :test '(a (b :default 5))))
	  (tmpl2 (make-template :test '())))
      (add-template env tmpl1)		; template defined to tmpl1
      (add-template env tmpl2)          ; template redefined to tmpl2
      (assert-eql (find-template env :test) tmpl2)
      (undo env)                        ; template should be tmpl1 again
      (assert-eql (find-template env :test) tmpl1)
      (undo env)			; template should be undefined
      (assert-false (find-template env :test))
      (redo env)		        ; template should be tmpl1 again
      (assert-eql (find-template env :test) tmpl1)
      (redo env)                        ; template should be tmpl2 again
      (assert-eql (find-template env :test) tmpl2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRATEGIES

(def-test-method undo-set-strategy ((tests undo-tests) :run nil)
  (with-slots (env) tests
    (set-strategy env :depth-strategy)
    (set-strategy env :breadth-strategy)
    (undo env)
    (assert-equal (current-strategy-name env) :depth-strategy)
    (redo env)
    (assert-equal (current-strategy-name env) :breadth-strategy)))

(def-test-method undo-add-strategy ((tests undo-tests) :run nil)
  (with-slots (env) tests
    (let ((str1 #'+) (str2 #'-))
      (add-strategy env :my-strategy str1)
      (add-strategy env :my-strategy str2)
      (assert-equal (eenv::find-strategy env :my-strategy) str2)
      (undo env)
      (assert-equal (eenv::find-strategy env :my-strategy) str1)
      (undo env)
      (assert-false (eenv::find-strategy env :my-strategy))
      (redo env)
      (assert-equal (eenv::find-strategy env :my-strategy) str1)
      (redo env)
      (assert-equal (eenv::find-strategy env :my-strategy) str2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACT GROUPS

(def-test-method undo-add-fact-group ((tests undo-tests) :run nil)
  (with-slots (env) tests
    (let ((fg1 (list :a)) (fg2 (list :b)))
      (add-fact-group env :my-fg fg1)
      (add-fact-group env :my-fg fg2)
      (assert-equal (find-fact-group env :my-fg) fg2)
      (undo env)
      (assert-equal (find-fact-group env :my-fg) fg1)
      (undo env)
      (assert-false (find-fact-group env :my-fg))
      (redo env)
      (assert-equal (find-fact-group env :my-fg) fg1)
      (redo env)
      (assert-equal (find-fact-group env :my-fg) fg2))))

(def-test-method undo-rem-fact-group ((tests undo-tests) :run nil)
  (with-slots (env) tests
    (let ((fg (list :a)))
      (add-fact-group env :my-fg fg)
      (rem-fact-group env :my-fg)
      (assert-false (find-fact-group env :my-fg))
      (undo env)
      (assert-equal (find-fact-group env :my-fg) fg)
      (redo env)
      (assert-false (find-fact-group env :my-fg)))))

(add-test-suite 'undo-tests)
;(textui-test-run (get-suite undo-tests))
