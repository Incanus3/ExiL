(in-package :exil-env)

(defgeneric print-undo-stack (env))
(defgeneric print-redo-stack (env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNDO/REDO

(defvar *undo-enabled* t)

(defmacro with-undo (env label undo-fun &body body)
  ;; redo function has the same body as the original action
  (let ((undo-fun-sym (gensym "undo-fun")))
    `(let ((,undo-fun-sym ,undo-fun))
       (prog1
	   (let ((*undo-enabled* nil))
	     ,@body)
	 (when *undo-enabled*
	   (reset-slots env (redo-stack))
	   (stack-for-undo ,env ,undo-fun-sym (lambda (env) ,@body) ,label))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun saving-forms (env-sym slot-names)
    (iter (for slot :in slot-names)
	  (collect `(,slot (,(symbol-append (list "copy-" slot) :exil-env)
                            (,slot ,env-sym))))))

  (defun setting-forms (env-sym slot-names)
    (iter (for slot :in slot-names)
	  (collect `(,slot ,env-sym))
	  (collect slot))))

(defmacro with-saved-slots (env slots undo-label &body body)
  (let ((env-sym (gensym "env")))
    `(let ((,env-sym ,env))
       (with-undo ,env-sym ,undo-label
	   (let ,(saving-forms env-sym slots)
	     (lambda (env) (setf ,@(setting-forms env slots))))
	 ,@body))))

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
      (let ((*undo-enabled* nil))
	(funcall redo-fun env))
      (stack-for-undo env undo-fun redo-fun label))))

(defun numbered-stack (stack)
  (numbered-map #'stack-item-label stack))

(defun print-stack (stack)
  (fresh-format t "~:{~5<~a: ~>~a~%~}" (numbered-stack stack)))

; public
(defmethod print-undo-stack ((env environment))
  (print-stack (undo-stack env)))

; public
(defmethod print-redo-stack ((env environment))
  (print-stack (redo-stack env)))
