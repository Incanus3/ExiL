(in-package :exil)

(defclass exil-environment ()
  ((facts :initform ())
   (templates :initform (make-hash-table))
   (rules :initform (make-hash-table))))

(defvar *environments*
  (let ((table (make-hash-table)))
    (setf (gethash "default" table)
	  (make-instance 'exil-environment))
    table))

(defvar *current-environment*
  (gethash "default" *environments*))

(defmacro defenv (name &key (redefine nil))
  (let ((name (my-symbol-name name)))
    (when (or (not (gethash name *environments*))
	      redefine)
      (setf (gethash name *environments*)
	    (make-instance 'exil-environment)))))

(defmacro setenv (name)
  (let ((env (gethash (my-symbol-name name) *environments*)))
    (when env (setf *current-environment* env))))

(defmacro exil-env-accessor (slot-name)
  `(progn
     (defun ,slot-name (&optional (environment *current-environment*))
       (slot-value environment ',slot-name))
     (defun (setf ,slot-name) (&optional (environment *current-environment*) value)
       (setf (slot-value environment ',slot-name) value))))

(defmacro exil-env-accessors (&rest slot-names)
  `(progn ,@(loop for slot-name in slot-names
	       collect `(exil-env-accessor ,slot-name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (exil-env-accessors facts templates rules))