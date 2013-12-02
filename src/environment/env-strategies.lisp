(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRATEGIES
                                        ; public
(defmethod add-strategy ((env environment) (name symbol) (function function)
			 &optional (undo-label "(add-strategy)"))
  (let ((original-function (find-strategy env name)))
    (unless (equalp function original-function)
      (with-undo env undo-label
	  (lambda (env) (add-strategy% env name original-function))
	(add-strategy% env name function)))))

(defmethod rem-strategy ((env environment) (name symbol)
			 &optional (undo-label "(rem-strategy)"))
  (let ((original-function (find-strategy env name)))
    (when original-function
      (with-undo env undo-label
	  (lambda (env) (add-strategy% env name original-function))
	(rem-strategy% env name)))))

(defun set-strategy-name (env name undo-label)
  (let ((original-strategy (current-strategy-name env)))
    (unless (equalp name original-strategy)
      (with-undo env undo-label
	  (lambda (env) (set-strategy-name% env original-strategy))
	(set-strategy-name% env name)))))

                                        ; public
(defmethod set-strategy ((env environment) &optional (name :default)
					     (undo-label "(set-strategy)"))
  (if (find-strategy env name)
      (set-strategy-name env name undo-label)
      (error "unknown strategy ~A" name)))
