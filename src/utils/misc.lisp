(in-package :exil-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TREES
(defun tree-find-all-if (pred tree)
  (if (listp tree)
      (mapcan (lambda (subtree)
		(tree-find-all-if pred subtree)) tree)
      (when (funcall pred tree) (list tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETS

(defun set-equal-p (set1 set2 &key (test #'eql))
  (null (set-exclusive-or set1 set2 :test test)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRINTING OUTPUT

(defun fresh-format (stream control-string &rest args)
  (fresh-line stream)
  (apply #'format stream control-string args))

(defun fresh-princ (object &optional stream)
  (fresh-line stream)
  (princ object stream)
  nil)
