(in-package :undo-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass undo-tests (test-case)
  ((env :accessor env)))

(defmethod set-up ((tests undo-tests))
  (with-slots (env) tests
    (setf env (exil-env:make-environment))))

(def-test-method undo-watch ((tests undo-tests))
  (with-slots (env) tests
    (unset-watcher env :facts) ; facts unwatched
    (set-watcher env :facts)   ; facts watched
    (undo env)                 ; facts should be unwatched again
    (assert-false (watched-p env :facts))
    (unset-watcher env :rules) ; rules unwatched
    (set-watcher env :all)     ; all watched
    (undo env)                 ; facts and rules should be unwatched again
    (assert-false (watched-p env :facts))
    (assert-false (watched-p env :rules))))
