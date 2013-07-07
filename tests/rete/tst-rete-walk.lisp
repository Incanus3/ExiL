(in-package :rete-tests)

(declaim (optimize (debug 3) (compilation-speed 0) (space 0) (speed 0)))

(defclass walk-graph-tests (test-case)
  ((graph :accessor graph)
   (n1 :accessor n1)
   (n2 :accessor n2)
   (n3 :accessor n3)))

(defclass graph-node () ((id :initform 0 :initarg :id :reader id)
			 (neighbors :initform () :initarg :neighbors
				    :accessor neighbors)))

(defmethod print-object ((node graph-node) stream)
  (prin1 (id node) stream))

(defmethod erete::neighbors append ((node graph-node))
  (slot-value node 'neighbors))

(defparameter *node-counter* 0)

(defun make-node (&optional neighbors)
  (incf *node-counter* 1)
  (make-instance 'graph-node :neighbors neighbors :id *node-counter*))

(defun connect (node1 node2)
  (push node1 (neighbors node2))
  (push node2 (neighbors node1)))

(defmethod set-up ((tests walk-graph-tests))
  (with-slots (graph n1 n2 n3) tests
    (setf n1 (make-node)
	  n2 (make-node)
	  n3 (make-node))
    (connect n1 n3)
    (connect n2 n3)
    (connect n1 n2)
    (setf (graph tests) n1)))

(def-test-method test-walk-graph ((tests walk-graph-tests) :run nil)
  (with-slots (graph n1 n2 n3) tests
    (assert-equal (erete::walk-graph graph)
		  `(,n1 (,n2 (,n1) (,n3 (,n2) (,n1))) (,n3)))
    (assert-equal (erete::graph->tree graph)
		  `(,n1 (,n2 (,n3))))
    (assert-equal (erete::graph->list graph)
		  `(,n1 ,n2 ,n3))))

(defclass walk-rete-tests (test-case)
  ((rule :accessor rule)
   (rete :accessor test-rete)))

(defmethod set-up ((tests walk-rete-tests))
  (with-slots (rule rete) tests
    (setf rule (make-rule :test-rule (list (make-simple-pattern '(in ?a ?b))) '())
	  rete (make-rete (make-instance 'env-mock)))
    (new-production rete rule)))
;; after adding rule to rete, rete should have following 9 nodes:
;; - alpha-top-node, alpha-subtop-node for simple facts
;; - 3 alpha-test-nodes for the 3 fields in the rule's condition
;; - alpha-memory-node connected to the last alpha-test-node
;; - beta-top-node, beta-join-node connected to the alpha-memory-node
;; - beta-memory-node connected to the beta-join-node, storing the rule
;; - which is satisfied, when this node is activated

(def-test-method test-rete-nodes ((tests walk-rete-tests) :run nil)
  (with-slots (rule rete) tests
    (let* ((nodes (erete::rete-nodes rete))
	   (partition (partition nodes #'type-of)))
      (assert-equal (length nodes) 9)
      (assert-equal (length (assoc-value 'exil-rete::alpha-top-node partition)) 1)
      (assert-equal (length (assoc-value 'exil-rete::alpha-subtop-node partition)) 1)
      (assert-equal (length (assoc-value 'exil-rete::alpha-test-node partition)) 3)
      (assert-equal (length (assoc-value 'exil-rete::alpha-memory-node partition)) 1)
      (assert-equal (length (assoc-value 'exil-rete::beta-top-node partition)) 1)
      (assert-equal (length (assoc-value 'exil-rete::beta-join-node partition)) 1)
      (assert-equal (length (assoc-value 'exil-rete::beta-memory-node partition)) 1))))

(add-test-suite 'walk-graph-tests)
(add-test-suite 'walk-rete-tests)
