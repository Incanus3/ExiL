(in-package :exil-rete)

(defgeneric weak-node-equal-p (node1 node2)
  (:documentation "compares nodes' static slots but doesn't recursively compare their children")
  (:method ((node1 node) (node2 node)) nil))

(defmethod weak-node-equal-p ((node1 alpha-top-node) (node2 alpha-top-node))
  t)

;; subtop-node of simple-facts part of alpha network has tmpl-name generated
;; by gensym (so that no user-defined template name can collide with it)
;; thus the test for gensymedp
(defmethod weak-node-equal-p ((node1 alpha-subtop-node)
			      (node2 alpha-subtop-node))
  (let ((tmpl-name1 (tmpl-name node1))
	(tmpl-name2 (tmpl-name node2)))
  (or (and (gensymedp tmpl-name1) (gensymedp tmpl-name2))
      (equal tmpl-name1 tmpl-name2))))

(defmethod weak-node-equal-p ((node1 alpha-test-node)
			      (node2 alpha-test-node))
  (and (equalp (tested-field node1)
               (tested-field node2))
       (constant-test (value node1)
                      (value node2))))

(defmethod weak-node-equal-p ((node1 alpha-memory-node)
			      (node2 alpha-memory-node))
  (exil-equal-p (pattern node1) (pattern node2))
  (set-equal-p (items node1) (items node2) :test #'exil-equal-p))

(defmethod weak-node-equal-p ((node1 beta-memory-node) (node2 beta-memory-node))
  (and (set-equal-p (items node1) (items node2) :test #'token-equal-p)
       (set-equal-p (productions node1) (productions node2) :test #'rule-equal-p)))

(defmethod weak-node-equal-p ((node1 beta-join-node) (node2 beta-join-node))
  (set-equal-p (tests node1) (tests node2) :test #'test-equal-p))

(defun neg-wmes-eql-p (neg-wmes1 neg-wmes2)
  (and (token-equal-p (car neg-wmes1) (car neg-wmes2))
       (set-equal-p (cdr neg-wmes1) (cdr neg-wmes2) :test #'exil-equal-p)))

(defmethod weak-node-equal-p ((node1 beta-negative-node) (node2 beta-negative-node))
  (and (set-equal-p (items node1) (items node2) :test #'token-equal-p)
       (set-equal-p (tests node1) (tests node2) :test #'test-equal-p)
       (set-equal-p (negative-wmes node1) (negative-wmes node2) :test #'neg-wmes-eql-p)))

(defun neighbors-equal-p (node1 node2 mapping)
  (let ((neighbors1 (neighbors node1))
	(neighbors2 (neighbors node2)))
    (and (= (length neighbors1) (length neighbors2))
	 (every (lambda (neigh1 neigh2)
		  (eql (assoc-value neigh1 mapping) neigh2))
		(neighbors node1) (neighbors node2)))))

(defun graph-copy-p (nodes1 nodes2)
  (and (= (length nodes1) (length nodes2))
       (every #'weak-node-equal-p nodes1 nodes2)
       (let ((mapping (mapcar #'cons nodes1 nodes2)))
	 (every (lambda (node1 node2)
		  (neighbors-equal-p node1 node2 mapping))
		nodes1 nodes2))))

;; returns true, if the two instances of rete have exactly the same structure
;; including the order of children, which is good enough for testing
;; correctness of copy-rete and of undo/redo functionality, that uses it
;;
;; should not be used as a general rete equality predicate as two rete
;; networks will behave equally even when the order of nodes' children
;; differs
(defun rete-copy-p (rete1 rete2)
  (graph-copy-p (rete-nodes rete1) (rete-nodes rete2)))
