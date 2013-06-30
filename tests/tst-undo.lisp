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
    
    ))

(add-test-suite 'undo-tests)
;(textui-test-run (get-suite undo-tests))
