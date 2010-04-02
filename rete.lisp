(in-package :exil)

#| exported methods:
(add-wme rete fact)
(remove-wme rete fact)
(add-production rete rule)
(remove-production rete rule)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general-purpose functions

(defun constant-test (desired-value real-value)
  (or (variable-p desired-value)
      (atom-equal-p desired-value real-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generalic node classes

(defclass node () ((children :accessor children :initform ())))

(defgeneric node-equal-p (node1 node2)
  (:method ((node1 node) (node2 node)) nil))

(defmethod add-child ((node node) (child node))
  (pushnew child (children node) :test #'node-equal-p)
  node)

;; probably redundant, there may be no need to add more then one child at a time
(defmethod add-children ((node node) (children list))
  (dolist (child children node)
    (add-child node child)))

(defgeneric node-activation (node object)
  (:documentation "handels various node activations"))

(defgeneric activate-children (node object)
  (:method ((node node) object)
    (dolist (child (children node))
      (node-activation child object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alpha memory classes

(defclass alpha-node (node) ())

(defclass alpha-test-node (alpha-node)
  ((tested-field :reader tested-field :initarg :tested-field
		 :initform (error "tested-field slot has to be specified"))
   (desired-value :reader value :initarg :value
		  :initform (error "desired-value slot has to be specified"))))

(defmethod node-equal-p ((node1 alpha-test-node)
			 (node2 alpha-test-node))
  (and (equalp (tested-field node1)
	       (tested-field node2))
       (atom-equal-p (value node1)
		     (value node2))))

(defgeneric test (node wme)
  (:documentation "provides testing part of alpha-test-node activation")
  (:method ((node alpha-test-node) (wme fact)) nil))

(defmethod activate-children ((node alpha-test-node) (wme fact))
  (dolist (child (children node))
    (when (node-activation child wme) (return))))

;; returns test return value, thanks to this it is possible for
;; activate-children to break after first successful test
(defmethod node-activation ((node alpha-test-node) (wme fact))
  (let ((test (test node wme)))
    (when test (activate-children node wme))
    test))

;; tested-field holds field index
(defclass simple-fact-test-node (alpha-test-node) ())

(defmethod test ((node simple-fact-test-node) (wme simple-fact))
  (constant-test (value node) (nth (tested-field node) (fact wme))))

;; tested-field holds field name
(defclass template-fact-test-node (alpha-test-node) ())

(defmethod test ((node template-fact-test-node) (wme template-fact))
  (constant-test (value node) (tmpl-fact-slot-value wme (tested-field node))))

;; slot dataflow-networks holds hash table of network top nodes in alpha memory.
;; for each template there is a dataflow network (accessible through
;; its template name) and one network is for simple-facts
;; slot simple-fact-key-name holds symbol for access into dataflow-networks
;; hash-table for simeple-facts, if there was some constant name for this,
;; it wouldn't be possible to create template of such name
(defclass alpha-top-node (alpha-node)
  ((dataflow-networks :accessor networks :initform (make-hash-table))
   (simple-fact-key-name :reader simple-fact-key-name
			 :initform (gensym "simple-fact"))))

(defmethod node-activation ((node alpha-top-node) (wme fact))
  (node-activation
   (gethash
    (typecase wme
      (simple-fact (simple-fact-key-name node))
      (template-fact (tmpl-name wme)))
    (networks node))
   wme))

;; children are beta-join-nodes
(defclass alpha-memory-node (alpha-node) ((items :accessor items :initform ())))

(defmethod node-activation ((node alpha-memory-node) (wme fact))
  (pushnew wme (items node) :test #'fact-equal-p)
  (activate-children node wme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beta memory classes

;; children are beta-memory-nodes (or production-nodes)
(defclass beta-join-node (node) ())

;(defclass tob-beta-node (beta-join-node) (pare 

;; children are beta-join-nodes
(defclass beta-memory-node (node) ((tokens :accessor tokens)))

(defclass production-node (beta-memory-node)
  ((production :reader production
	       :initarg :production
	       :initform (error "production slot has to be specified"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compound rete class and methods for export

(defclass rete () ((top-alpha-node :reader top-a-node
				   :initform (make-instance 'alpha-top-node))
		   (top-beta-node  :reader   top-b-node)))

(defmethod add-wme ((rete rete) (fact fact))
  (declare (ignore rete fact))

  )

(defmethod remove-wme ((rete rete) (fact fact))
  (declare (ignore rete fact))

  )

(defmethod add-production ((rete rete) (rule rule))
  (declare (ignore rete rule))
  
  )

(defmethod remove-production ((rete rete) (rule rule))
  (declare (ignore rete rule))

  )
