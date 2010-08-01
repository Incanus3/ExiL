(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alpha memory classes

(defclass alpha-node (node) ())

(defclass alpha-test-node (alpha-node)
  ((tested-field :reader tested-field :initarg :tested-field
		 :initform (error "tested-field slot has to be specified"))
   (desired-value :reader value :initarg :value
		  :initform (error "desired-value slot has to be specified"))
   (alpha-memory :accessor memory :initarg :memory
		 :initform nil)))

(defmethod node-equal-p ((node1 alpha-test-node)
			 (node2 alpha-test-node))
  (and (equalp (tested-field node1)
	       (tested-field node2))
       (constant-test (value node1)
		      (value node2))))

(defmethod print-object ((node alpha-test-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "| field: ~A, value: ~A" (tested-field node) (value node))))

(defgeneric test (node wme)
  (:documentation "provides testing part of alpha-test-node activation")
  (:method ((node alpha-test-node) (wme fact)) nil))

;; once the wme passes test of some of node's children, there's no need
;; to continue the search, because the children are created in a way
;; that no wme can pass 2 children's tests
(defmethod activate-children ((node alpha-test-node) (wme fact))
  (dolist (child (children node))
    (activate child wme)))

(defmethod activate-memory ((node alpha-test-node) (wme fact))
  (with-slots ((mem alpha-memory)) node
    (when mem
      (activate mem wme))))

;; returns test return value, thanks to this it is possible for
;; activate-children to break after first successful test
(defmethod activate ((node alpha-test-node) (wme fact))
  (let ((test (test node wme)))
    (when test
      (activate-children node wme)
      (activate-memory node wme))
    test))

(defmethod inactivate :after ((node alpha-test-node) (wme fact))
  (when (memory node)
    (inactivate (memory node) wme)))

(defclass simple-fact-alpha-node (alpha-node) ())

(defclass template-fact-alpha-node (alpha-node) ())

;; tested-field holds field index
(defclass simple-fact-test-node (alpha-test-node simple-fact-alpha-node) ())

(defmethod test ((node simple-fact-test-node) (wme simple-fact))
  (constant-test (value node) (nth (tested-field node) (fact wme))))

;; tested-field holds field name
(defclass template-fact-test-node (alpha-test-node template-fact-alpha-node) ())

(defmethod test ((node template-fact-test-node) (wme template-fact))
  (constant-test (value node) (tmpl-fact-slot-value wme (tested-field node))))

;; after alpha-top-node selects the right dataflow network according to fact type
;; and eventually template name, it activates the right subtop node
;; the simple-fact subtop-node is created when during rete class initialization
;; the template-fact subtop-nodes are created, when condition of that template
;; appears in some newly added rule
(defclass alpha-subtop-node (alpha-node) ())

(defmethod activate ((node alpha-subtop-node) (wme fact))
  (activate-children node wme))

(defclass simple-fact-subtop-node (alpha-subtop-node simple-fact-alpha-node) ())

(defclass template-fact-subtop-node (alpha-subtop-node template-fact-alpha-node) ())

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

;(defmethod initialize-instance :after ((node alpha-top-node) &key)
;  (setf (gethash (simple-fact-key-name node)
;		 (networks node))
;	(make-instance 'simple-fact-subtop-node)))

;; OR GET-NETWORK INITIALIZE-SUBNET

(defmethod get-network ((node alpha-top-node)
			&optional (template-name (simple-fact-key-name node)))
  (gethash template-name (networks node)))

(defmethod (setf get-network) (value (node alpha-top-node)
			       &optional (template-name (simple-fact-key-name node)))
  (setf (gethash template-name (networks node)) value))

(defmethod initialize-network ((node alpha-top-node)
			      &optional (template-name (simple-fact-key-name node)))
  (setf (get-network node template-name)
	(make-instance (if (equalp template-name (simple-fact-key-name node))
			   'simple-fact-subtop-node
			   'template-fact-subtop-node))))

(defmethod get/initialize-network ((node alpha-top-node)
				   &optional (template-name (simple-fact-key-name node)))
  (or (get-network node template-name)
      (initialize-network node template-name)))

(defmethod initialize-instance :after ((node alpha-top-node) &key)
  (initialize-network node))

(defmethod activate ((node alpha-top-node) (wme fact))
  (activate
   (gethash
    (typecase wme
      (simple-fact (simple-fact-key-name node))
      (template-fact (tmpl-name wme)))
    (networks node))
   wme))

(defmethod inactivate ((node alpha-top-node) (wme fact))
  (loop for child being the hash-values in (networks node)
     do (inactivate child wme)))

;; children are beta-join-nodes
(defclass alpha-memory-node (alpha-node memory-node) ())

(defmethod activate ((node alpha-memory-node) (wme fact))
  (pushnew wme (items node) :test #'fact-equal-p)
  (activate-children node wme))

(defmethod inactivate ((node alpha-memory-node) (wme fact))
  (setf (items node) (delete wme (items node) :test #'fact-equal-p)))

