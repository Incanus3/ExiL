(in-package :exil-env)

(defgeneric undo (env))

(defmethod undo ((env environment))
  (when (undo-stack env)
    (funcall (pop (undo-stack env)))))

(defun stack-for-undo (env fun)
  (push fun (undo-stack env)))

(defmacro with-undo (env undo-fun &body body)
  `(progn (stack-for-undo ,env ,undo-fun)
	  ,@body))
