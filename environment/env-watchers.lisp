(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WATCHERS

(defun watched-p (env watcher)
  (assoc-value watcher (watchers env)))

(defun is-watcher (env watcher)
  (assoc watcher (watchers env)))

(defun assert-watcher (env watcher)
  (cl:assert (or (equalp watcher :all)
                 (is-watcher env watcher))
             () "I don't know how to watch ~A" watcher))

(defun set-one-watcher% (env watcher val)
  (setf (assoc-value watcher (watchers env)) val))

(defun set-one-watcher (env watcher val)
  (with-undo env
      (let ((original-value (watched-p env watcher)))
	(lambda ()
	  (set-one-watcher% env watcher original-value)))
    (set-one-watcher% env watcher val)))

(defun set-all-watchers (env val)
  (with-undo env
      (let ((original-watchers (watchers env)))
	(lambda () (setf (watchers env) original-watchers)))
    (setf (watchers env) (mapcar (lambda (pair) (cons (car pair) val))
				 (watchers env)))))

; public
(defmethod set-watcher ((env environment) (watcher symbol))
  (let ((name (to-keyword watcher)))
    (assert-watcher env name)
    (if (equalp name :all)
        (set-all-watchers env t)
	(set-one-watcher env name t))))

; public
(defmethod unset-watcher ((env environment) (watcher symbol))
  (let ((name (to-keyword watcher)))
    (assert-watcher env name)
    (if (equalp name :all)
        (set-all-watchers env nil)
	(set-one-watcher env name nil))))
