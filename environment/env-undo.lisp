(in-package :exil-env)

(defgeneric print-undo-stack (env))
(defgeneric print-redo-stack (env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNDO/REDO

(defmacro with-undo (env label undo-fun &body body)
  ;; redo function has the same body as the original action
  (let ((undo-fun-sym (gensym "undo-fun")))
    `(let ((,undo-fun-sym ,undo-fun))
       (prog1
	 ,@body
	 (stack-for-undo ,env ,undo-fun-sym (lambda (env) ,@body) ,label)))))

; public
(defmethod undo ((env environment))
  (when (undo-stack env)
    (pop-undo (undo-fun redo-fun label) env
      (funcall undo-fun env)
      (stack-for-redo env redo-fun undo-fun label))))

; public
(defmethod redo ((env environment))
  (when (redo-stack env)
    (pop-redo (redo-fun undo-fun label) env
      (funcall redo-fun env)
      (stack-for-undo env undo-fun redo-fun label))))

(defun numbered-stack (stack)
  (numbered-map #'stack-item-label stack))

(defun print-stack (stack)
  (format t "~:{~5<~a: ~>~a~%~}" (numbered-stack stack)))

; public
(defmethod print-undo-stack ((env environment))
  (fresh-line)
  (print-stack (undo-stack env)))

; public
(defmethod print-redo-stack ((env environment))
  (fresh-line)
  (print-stack (redo-stack env)))
