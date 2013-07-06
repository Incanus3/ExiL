(in-package :exil-rete)

(defgeneric walk-rete (rete &key memo-fun aggreg before))
(defgeneric rete-nodes (rete &optional type))


(defgeneric neighbors (node)
  (:method-combination append)
  (:method append ((null null)) ()))

(defun getmemo (node memo)
  (gethash node memo :not-found))

(defun memoize (node memo value)
  (setf (gethash node memo) value))

(defmacro with-memoize ((node memo memo-fun) &body body)
  (let ((memo-result-sym (gensym "memoized result"))
	(node-sym (gensym "node"))
	(memo-sym (gensym "memo")))
    `(let* ((,node-sym ,node)
	    (,memo-sym ,memo)
	    (,memo-result-sym (getmemo ,node-sym ,memo-sym)))
       (if (not (equal ,memo-result-sym :not-found)) ,memo-result-sym
	   (progn
	     (memoize ,node-sym ,memo-sym (funcall ,memo-fun ,node-sym))
	     ,@body)))))

(defun visit (node memo memo-fun aggreg before)
  (with-memoize (node memo memo-fun)
    (funcall before node)
    (funcall aggreg node (mapcar (lambda (node)
				   (visit node memo memo-fun aggreg before))
				 (neighbors node)))))

(defun neighbors-equal-p (node1 node2 mapping)
  (every (lambda (neigh1 neigh2)
	   (eql (assoc-value neigh1 mapping) neigh2))
	 (neighbors node1) (neighbors node2)))

(defun copy-equal-p (nodes1 nodes2)
  (and (= (length nodes1) (length nodes2))
       (every #'weak-node-equal-p nodes1 nodes2)
       (let ((mapping (mapcar #'cons nodes1 nodes2)))
	 (every (lambda (node1 node2)
		  (neighbors-equal-p node1 node2 mapping))
		nodes1 nodes2))))

;; memo-fun takes a node and what it returns is memoized for that node,
;; so it's returned by visit when this node is visited again
;; aggreg takes node and a list of visit return values for neighbors and
;; what it returns is returned when the node is visited the first time
;; thus if we used e.g. (constantly nil) as memo-fun, we can now remove nils
;; from the list in aggreg

;; therefore :memo-fun #'list :aggreg #'cons
;; will result in a tree where each node appears as (node . <children>) when
;; visited for the fist time and as (node) when visited subsequently, e.g.:
;; (1 (3 (1) (2 (3) (1))) (2))
;; this is the default as it shows the full structure of the graph - when
;; I see (1) I can look up and find out that 1 has 3 and 2 for neighbors

;; :memo-fun (constantly nil) :aggreg #'cons
;; will result in a tree where each node appears as (node . <children>) when
;; visited for the first time and as nil when visited subsequently, e.g:
;; (1 (3 NIL (2 NIL NIL)) NIL)

;; :memo-fun (constantly nil)
;; :aggreg (lambda (node children) (cons node (remove nil children)))
;; will result in a tree with each node appearing exactly once, e.g:
;; (1 (3 (2)))

;; finaly :memo-fun (constantly nil)
;; :aggreg (lambda (node children) (cons node (apply #'append children)))
;; will result in a plain list of nodes
(defun walk-graph (start-node &key (memo-fun #'list) (aggreg #'cons)
				(before (constantly nil)))
  (let ((memo (make-hash-table)))
    (visit start-node memo memo-fun aggreg before)))

(defun graph->tree (start-node)
  (walk-graph start-node :memo-fun (constantly nil)
	      :aggreg (lambda (node children)
			(cons node (remove nil children)))))

(defun graph->list (start-node)
  (walk-graph start-node :memo-fun (constantly nil)
	      :aggreg (lambda (node children)
			(cons node (apply #'append children)))))

(defmethod neighbors append ((node node))
  (children node))

(defmethod neighbors append ((node alpha-top-node))
  (hash-values (networks node)))

(defmethod neighbors append ((node alpha-test-node))
  (to-list (memory node)))

(defmethod neighbors append ((node beta-node))
  (to-list (parent node)))

(defmethod neighbors append ((node beta-join-node))
  (to-list (alpha-memory node)))


(defmethod walk-rete ((rete rete) &key (memo-fun #'list) (aggreg #'cons)
				    (before (constantly nil)))
  (walk-graph (alpha-top-node rete) :memo-fun memo-fun :aggreg aggreg :before before))

(defmethod rete-nodes ((rete rete) &optional type)
  (let ((nodes (graph->list (alpha-top-node rete))))
    (if type
	(remove-if-not (lambda (node) (typep node type)) nodes)
	nodes)))

;; returns true, if the two instances of rete have exactly the same structure
;; including the order of children, which is good enough for testing
;; correctness of copy-rete and of undo/redo functionality, that uses it
;;
;; should not be used as a general rete equality predicate as two rete
;; networks will behave equally even when the order of nodes' children
;; differs
(defmethod rete-copy-equal-p ((rete1 rete) (rete2 rete))
  (copy-equal-p (rete-nodes rete1) (rete-nodes rete2)))
