(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rete classes

(defclass node () ((children :accessor children :initform nil)))

(defgeneric node-equal-p (node1 node2)
  (:method ((node1 node) (node2 node)) nil))

(defmethod add-child ((node node) (child node))
  (pushnew child (children node) :test #'node-equal-p)
  node)

;; probably redundant, there may be no need to add more then one child at a time
(defmethod add-children ((node node) (children list))
  (dolist (child children node)
    (add-child node child)))

(defclass alpha-test-node (node)
  ((pattern :reader pattern
	    :initarg :pattern
	    :initform (error "alpha-test-node pattern has to be specified"))))

(defmethod node-equal-p ((node1 alpha-test-node) (node2 alpha-test-node))
  (pattern-equal-p (pattern node1) (pattern node2)))

(defclass alpha-memory-node (node) ((fact-bindings-pairs :accessor fb-pairs)))

(defclass beta-join-node (node) ())

;(defmethod 

(defclass beta-memory-node (node) ((tokens :accessor tokens)))

(defclass rete () ((alpha-test-nodes   :accessor a-test-nodes)
		   (alpha-memory-nodes :accessor a-mem-nodes)
		   (beta-join-nodes    :accessor b-join-nodes
				       :initform (make-instance 'beta-join-node))
		   (beta-memory-nodes  :accessor b-mem-nodes)))

(defmethod add-rule ((rete rete) (rule rule))
  (let ((subsets (subsets (conditions rule))))
    )
  )
