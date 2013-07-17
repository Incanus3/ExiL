(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WATCHERS

(defun assert-watcher (env watcher)
  (cl:assert (or (equalp watcher :all)
                 (is-watcher env watcher))
             () "I don't know how to watch ~A" watcher))

(defun set-one-watcher (env watcher val undo-label)
  (let ((original-value (watched-p env watcher)))
    (unless (equal original-value val)
      (with-undo env undo-label
	  (lambda (env)
	    (set-one-watcher% env watcher original-value))
	(set-one-watcher% env watcher val)))))

(defun all-watchers-set-to (env val)
  (every (lambda (watcher) (equalp (cdr watcher) val)) (watchers env)))

(defun set-all-watchers (env val undo-label)
  (unless (all-watchers-set-to env val)
    (with-saved-slots env (watchers) undo-label
      (set-all-watchers% env val))))

; public
(defmethod set-watcher ((env environment) (watcher symbol)
			&optional (undo-label "(set-watcher)"))
  (let ((name (to-keyword watcher)))
    (assert-watcher env name)
    (if (equalp name :all)
        (set-all-watchers env t undo-label)
	(set-one-watcher env name t undo-label)))
  nil)

; public
(defmethod unset-watcher ((env environment) (watcher symbol)
			  &optional (undo-label "(unset-watcher)"))
  (let ((name (to-keyword watcher)))
    (assert-watcher env name)
    (if (equalp name :all)
        (set-all-watchers env nil undo-label)
	(set-one-watcher env name nil undo-label)))
  nil)
