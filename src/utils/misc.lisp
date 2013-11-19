(in-package :exil-utils)

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
