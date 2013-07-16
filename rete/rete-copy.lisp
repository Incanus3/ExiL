(in-package :exil-rete)

; public
(defgeneric copy-rete (rete &optional new-env))
(defgeneric copy-node (node memo)
  (:method ((node null) memo) nil))

(defun with-2nd-param (fun param)
  "parially applies fun with param as second parameter"
  (lambda (x) (funcall fun x param)))


(defun copy-children (node memo)
  (mapcar (with-2nd-param #'copy-node memo) (children node)))

(defun memoized-node (memo old-node)
  (gethash old-node memo))

(defun memoize-node (memo old-node new-node)
  (setf (gethash old-node memo) new-node))

(defmethod copy-node ((node node) memo)
  (let ((new-node (make-instance (class-of node))))
    (memoize-node memo node new-node)
    (setf (children new-node) (copy-children node memo))
    new-node))

(defmethod copy-node ((node memory-node) memo)
  (let ((new-node (call-next-method)))
    (setf (items new-node) (copy-list (items node)))
    new-node))

(defun copy-networks (networks memo)
  (map-hash-table (with-2nd-param #'copy-node memo) networks))

(defmethod copy-node ((node alpha-top-node) memo)
  (let ((new-node (call-next-method)))
    (setf (networks new-node) (copy-networks (networks node) memo)
	  (simple-fact-key new-node) (simple-fact-key node))
    new-node))

(defmethod copy-node ((node alpha-subtop-node) memo)
  (let ((new-node (call-next-method)))
    (setf (tmpl-name new-node) (tmpl-name node))
    new-node))

(defmethod copy-node ((node alpha-test-node) memo)
  (let ((new-node (call-next-method)))
    (with-slots (tested-field desired-value alpha-memory) new-node
      (setf tested-field (tested-field node)
	    desired-value (value node)
	    alpha-memory (copy-node (memory node) memo))
      new-node)))

(defmacro with-memo ((node memo) &body body)
  (let ((mem-node-sym (gensym "memoized-node")))
    `(let ((,mem-node-sym (memoized-node ,memo ,node)))
       (if ,mem-node-sym ,mem-node-sym
	   (progn ,@body)))))

(defmethod copy-node ((node alpha-memory-node) memo)
  (with-memo (node memo)
    (let ((new-node (call-next-method)))
      (setf (pattern new-node) (pattern node))
      new-node)))

(defmethod copy-node ((node beta-node) memo)
  (let ((new-node (call-next-method)))
    (setf (parent new-node) (copy-node (parent node) memo))
    new-node))

(defmethod copy-node ((node beta-join-node) memo)
  (with-memo (node memo)
    (let ((new-node (call-next-method)))
      (setf (alpha-memory new-node) (copy-node (alpha-memory node) memo)
	    (tests new-node) (tests node))
      new-node)))

(defmethod copy-node ((node beta-memory-node) memo)
  (with-memo (node memo)
    (let ((new-node (call-next-method)))
      (setf (productions new-node) (copy-list (productions node)))
      (setf (rete new-node) (gethash :rete memo))
      new-node)))

(defmethod copy-node ((node beta-negative-node) memo)
  (with-memo (node memo)
    (let ((new-node (call-next-method)))
      (setf (negative-wmes new-node) (copy-tree (negative-wmes node)))
      new-node)))

(defmethod copy-node ((node beta-top-node) memo)
  (with-memo (node memo)
    (let ((new-node (call-next-method)))
      (setf (gethash :beta-top memo) new-node)
      new-node)))

(defun copy-rete% (alpha-top beta-top new-rete)
  (let ((memo (make-hash-table)))
    (setf (gethash :rete memo) new-rete)
    (let ((new-alpha-top (copy-node alpha-top memo)))
      (list new-alpha-top
	    ;; rete network is continuous, we could get from alpha-top
	    ;; to beta-top
	    (or (gethash :beta-top memo)
		;; there aren't any rules yet, so alpha and beta parts
		;; aren't connected
		(copy-node beta-top memo))))))

; public
(defmethod copy-rete ((rete rete) &optional (new-env (environment rete)))
  (let ((new-rete (make-rete new-env)))
    (with-slots (alpha-top-node beta-top-node) new-rete
      (destructuring-bind (new-alpha-top new-beta-top)
	  (copy-rete% (alpha-top-node rete) (beta-top-node rete) new-rete)
	(setf alpha-top-node new-alpha-top beta-top-node new-beta-top)))
    new-rete))
