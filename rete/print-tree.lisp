(in-package :exil)

(defun print-node (node depth &optional (stream t))
  (dotimes (i (1- depth))
    (format stream " "))
  (format stream " ~[*-- ~:; `-- ~]~A~%" depth node))

(defmethod node-children ((node erete::node))
  (erete::children node))

(defun hash-values (hash-table)
  (loop for val being the hash-values in hash-table
     collect val))

(defmethod node-children ((node erete::alpha-top-node))
  (hash-values (erete::networks node)))

(defmethod node-children ((node erete::alpha-test-node))
  (if (erete::memory node)
      (cons (erete::memory node) (erete::children node))
      (erete::children node)))

;(defmethod node-children ((node beta-join-node))
;  (if (memory node)
;      (cons (memory node) (children node))
;      (children node)))

;; preorder, not really a search, but that's the official name
(defun depth-first-search (root function &optional (depth 0))
  (funcall function root depth)
  (dolist (child (node-children root))
    (depth-first-search child function (1+ depth))))

(defun print-rete (&optional (stream t) (rete (eenv::rete)))
  (format stream "alpha-part:~%")
  (depth-first-search (erete::alpha-top-node rete)
		      #'print-node)
  (format stream "beta-part:~%")
  (depth-first-search (erete::beta-top-node rete)
		      #'print-node))
