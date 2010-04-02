(in-package :exil)

#| exported methods:
(add-wme rete fact)
(remove-wme rete fact)
(add-production rete rule)
(remove-production rete rule)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general-purpose classes

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

(defgeneric activate-children (node)
  (:method ((node node))
    (dolist (child (children node))
      (node-activation child))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alpha memory classes

(defclass alpha-node (node) ())

(defclass alpha-test-node (alpha-node)
  ((tested-field :reader tested-field :initarg :tested-field
		 :initform (error "tested-field slot has to be specified"))
   (desired-value :reader value :initarg :value
		  :initform (error "desired-value slot has to be specified"))))

;; slot dataflow-networks holds hash table of network top nodes in alpha memory.
;; for each template there is a dataflow network (accessible through
;; its template name) and one network is for simple-facts
;; slot simple-fact-key-name holds symbol for access into dataflow-networks
;; hash-table for simeple-facts, if there was some constant name for this,
;; it wouldn't be possible to create template of such name
(defclass alpha-top-node (alpha-test-node)
  ((dataflow-networks :accessor networks :initform (make-hash-table))
   (simple-fact-key-name :reader simple-fact-key-name
			 :initform (gensym "simple-fact"))))

(defmethod node-activation ((node alpha-test-node) (wme fact))
  (node-activation
   (gethash
    (typecase wme
      (simple-fact (simple-fact-key-name node))
      (template-fact (tmpl-name wme)))
    (networks node))))

(defgeneric test (node wme)
  (:documentation "provides testing part of alpha-test-node activation")
  (:method ((node alpha-test-node) wme) nil))

(defmethod node-activation ((node simple-fact-test-node) (wme simple-fact))
  (when (test node wme) (activate-children node)))

;; tested-field holds field index
(defclass simple-fact-test-node (alpha-test-node) ())

(defmethod test ((node simple-fact-test-node) (wme simple-fact))
  (atom-equal-p (nth (tested-field node) (fact wme))
		(value node)))

;; tested-field holds field name
(defclass template-fact-test-node (alpha-test-node) ())

(defmethod test ((node template-fact-test-node) (wme template-fact))
  (atom-equal-p (tmpl-fact-slot-value wme (tested-field node))
		(value node)))

;; children are beta-join-nodes
(defclass alpha-memory-node (node) ((items :accessor items :initform ())))

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

(defclass rete () ((top-alpha-node :reader top-a-node)
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
