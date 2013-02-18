(in-package :exil-rete)

(defgeneric node-children (node))
(defgeneric print-rete (rete &optional stream))

(defun print-node (node depth &optional (stream t))
  (dotimes (i (1- depth))
    (format stream " "))
  (format stream " ~[*-- ~:; `-- ~]~A~%" depth node))

(defmethod node-children ((node node))
  (children node))

(defun hash-values (hash-table)
  (iter (for (key val) in-hashtable hash-table)
        (collect val)))

(defmethod node-children ((node alpha-top-node))
  (hash-values (networks node)))

(defmethod node-children ((node alpha-test-node))
  (if (memory node)
      (cons (memory node) (children node))
      (children node)))

;(defmethod node-children ((node beta-join-node))
;  (if (memory node)
;      (cons (memory node) (children node))
;      (children node)))

;; preorder, not really a search, but that's the official name
(defun depth-first-search (root function &optional (depth 0))
  (funcall function root depth)
  (dolist (child (node-children root))
    (depth-first-search child function (1+ depth))))

(defmethod print-rete ((rete rete) &optional (stream t))
  (format stream "alpha-part:~%")
  (depth-first-search (alpha-top-node rete)
		      #'print-node)
  (format stream "beta-part:~%")
  (depth-first-search (beta-top-node rete)
		      #'print-node))
