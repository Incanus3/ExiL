(in-package :exil-utils)

(defun every-couple% (predicate list)
  "applies 2-parameter predicate to every couple of items in the list,
   returns true if all the return values are true"
  (when (evenp (length list))
    (iter (with lst-copy = (copy-list list))
          (while lst-copy)
          (for first = (pop lst-copy))
          (for second = (pop lst-copy))
          (unless (funcall predicate first second) (return nil))
          (finally (return t)))))

(defun plistp (list)
  "is the list a property list?"
  (every-couple% (lambda (key val)
                   (declare (ignore val))
                   (keywordp key))
                 list))

(defmacro doplist ((key val plist &optional (retval nil)) &body body)
  "iterates over plist setting key and val variables for each iteration"
  (let ((sym-plist (gensym "plist")))
    `(let ((,sym-plist (copy-list ,plist)))
       (iter (for ,key = (pop ,sym-plist))
             (for ,val = (pop ,sym-plist))
             (while ,key)
             ,@body
             (finally (return ,retval))))))
