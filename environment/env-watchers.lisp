(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WATCHERS

(defun assert-watcher (env watcher)
  (cl:assert (or (equalp watcher :all)
                 (is-watcher env watcher))
             () "I don't know how to watch ~A" watcher))

(defun set-one-watcher (env watcher val undo-label)
  (with-undo env undo-label
      (let ((original-value (watched-p env watcher)))
	(lambda (env)
	  (set-one-watcher% env watcher original-value)))
    (set-one-watcher% env watcher val)))

;; it would seem that original-watchers should store a copy of the watchers
;; alist, but this is not necessary, as set-all-watchers% creates new alist
;; from scratch, so the conses are not reused
(defun set-all-watchers (env val undo-label)
  (with-undo env undo-label
      (let ((original-watchers (watchers env)))
	(lambda (env) (setf (watchers env) original-watchers)))
    (set-all-watchers% env val)))

; public
(defmethod set-watcher ((env environment) (watcher symbol)
			&optional (undo-label "(watch)"))
  (let ((name (to-keyword watcher)))
    (assert-watcher env name)
    (if (equalp name :all)
        (set-all-watchers env t undo-label)
	(set-one-watcher env name t undo-label))))

; public
(defmethod unset-watcher ((env environment) (watcher symbol)
			  &optional (undo-label "(unwatch)"))
  (let ((name (to-keyword watcher)))
    (assert-watcher env name)
    (if (equalp name :all)
        (set-all-watchers env nil undo-label)
	(set-one-watcher env name nil undo-label))))
