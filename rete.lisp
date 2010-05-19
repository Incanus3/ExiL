(in-package :exil)

#| exported methods:
(add-wme rete fact)
(remove-wme rete fact)
(add-production rete rule)
(remove-production rete rule)
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fact matching

;; terminology note:
;; be sure about distiction of following expressions
;; a) template-facts - instances of class template-fact
;;      - they're just facts with named slots
;;      - e.g. (car :color red :mph 160)
;; b) templates - instances of class template
;;      - prescriptions for template-facts - describe the slot names,
;;        default values, etc.
;;      - you can't create template-fact without having a template for it
;; c) patterns - could be either in simple-fact or in template-fact form
;;      - they're not facts, they can include wildcards, variables, etc.
;;      - e.g. (on red-box ?some-other-box)
;;      - or (car :color ?some-color)

(defgeneric atom-equal-p (object1 object2)
  (:documentation "equality predicate for fact atoms")
  (:method (object1 object2) (equalp object1 object2)))

(defgeneric variable-bindings (pattern fact)
  (:documentation "if the fact passes the pattern,
                     returns the variable bindings in assoc list"))

;; checks consistency of constant atoms in pattern, ignores variables
;; returns fact if passed, nil otherwise
(defmethod constant-check ((pattern simple-pattern)
			   (fact simple-fact))
  (when (every (lambda (pt-atom atom)
		 (or (variable-p pt-atom)
		     (atom-equal-p pt-atom atom)))
	       (pattern pattern)
	       (fact fact))
    fact))

;; could use hash-table instead of assoc-list, if the facts has many atoms
;; in need of longer facts could be 2 methods (with assoc-list and hash table)
;; and one that would call them depending on length
(defmethod variable-bindings ((pattern simple-pattern)
			      (fact simple-fact))
  (loop
     with bindings = ()
     for pt-atom in (pattern pattern)
     for atom in (fact fact)
     do (if (variable-p pt-atom)
	    (let ((binding (cdr (assoc pt-atom bindings))))
	      (if binding
		  (unless (atom-equal-p binding atom)
		    (return nil))
		  (push (cons pt-atom atom) bindings)))
	    (unless (atom-equal-p pt-atom atom)
	      (return nil)))
     finally (return (nreverse bindings))))

(defun constant-test (desired-value real-value)
  (or (variable-p desired-value)
      (atom-equal-p desired-value real-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic node classes

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

(defclass memory-node (node) ((items :accessor items :initform ())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alpha memory classes

(defclass alpha-node (node) ())

(defclass alpha-test-node (alpha-node)
  ((tested-field :reader tested-field :initarg :tested-field
		 :initform (error "tested-field slot has to be specified"))
   (desired-value :reader value :initarg :value
		  :initform (error "desired-value slot has to be specified"))
   (alpha-memory :reader memory :initarg :memory
		 :initform nil)))

(defmethod node-equal-p ((node1 alpha-test-node)
			 (node2 alpha-test-node))
  (and (equalp (tested-field node1)
	       (tested-field node2))
       (constant-test (value node1)
		      (value node2))))

(defgeneric test (node wme)
  (:documentation "provides testing part of alpha-test-node activation")
  (:method ((node alpha-test-node) (wme fact)) nil))

;; once the wme passes test of some of node's children, there's no need
;; to continue the search, because the children are created in a way
;; that no wme can pass 2 children's tests
(defmethod activate-children ((node alpha-test-node) (wme fact))
  (dolist (child (children node))
    (when (node-activation child wme) (return))))

(defmethod activate-memory ((node alpha-test-node) (wme fact))
  (with-slots ((mem memory)) node
    (when mem
      (node-activation mem wme))))

;; returns test return value, thanks to this it is possible for
;; activate-children to break after first successful test
(defmethod node-activation ((node alpha-test-node) (wme fact))
  (let ((test (test node wme)))
    (when test
      (activate-children node wme)
      (activate-memory node wme))
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
(defclass alpha-memory-node (alpha-node memory-node) ())

(defmethod node-activation ((node alpha-memory-node) (wme fact))
  (pushnew wme (items node) :test #'fact-equal-p)
  (activate-children node wme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beta memory classes

(defclass beta-node (node) ((parent :accessor parent :initarg :parent
				    :initform nil)))

;; could be changed to cons to be more effective
(defclass token () ((parent :reader parent :initarg :parent :initform nil)
		    (wme :reader wme :initarg :wme
			 :initform (error "wme slot has to be specified"))))

(defmethod previous-wme ((token token) &optional (n 1))
  "gives wme from token n wmes back"
  (dotimes (i n (wme token))
    (setf token (parent token))))

(defgeneric token-equal-p (token1 token2)
  (:documentation "token equality predicate")
  (:method (token1 token2) nil)
  (:method ((token1 (eql nil)) (token2 (eql nil))) t)
  (:method ((token1 token) (token2 token))
    (and (fact-equal-p (wme token1) (wme token2))
	 (token-equal-p (parent token1) (parent token2)))))

;; children are beta-join-nodes
(defclass beta-memory-node (beta-node memory-node) ())

(defmethod node-activation ((node beta-memory-node) (token token))
  (pushnew token (items node) :test #'token-equal-p)
  (activate-children node token))

(defclass production-node (beta-memory-node)
  ((production :reader production
	       :initarg :production
	       :initform (error "production slot has to be specified"))))

(defclass test () ((current-field-to-test
		    :reader current-field :initarg :current-field
		    :initform (error "current-field slot has to be specified"))
		   (previous-condition-number
		    :documentation "tells, how many conditions back i must go"
		    :reader previous-condition :initarg :previous-condition
		    :initform 0)
		   (previous-field-to-test
		    :reader previous-field :initarg :previous-field
		    :initform (error "previous-field slot has to be specified"))))

;; children are beta-memory-nodes (or production-nodes)
(defclass beta-join-node (beta-node)
  ((alpha-memory :reader memory :initarg :memory
		 :initform (error "alpha-memory slot has to be specified"))
   (tests :accessor tests :initarg :tests :initform ())))

(defmethod perform-join-test ((test test) (token token) (wme fact))
  (atom-equal-p (fact-field wme (current-field test))
		(fact-field (previous-wme token (previous-condition test))
			    (previous-field test))))

(defmethod perform-join-tests ((tests list) (token token) (wme fact))
  (dolist (test tests t)
    (unless (perform-join-test test token wme) (return nil))))
    
;; left activation
(defmethod node-activation ((node beta-join-node) (token token))
  (dolist (wme (items (memory node)))
    (if (perform-join-tests (tests node) token wme)
	(activate-children
	 node (make-instance 'token :parent token :wme wme)))))

;; right activation
(defmethod node-activation ((node beta-join-node) (wme fact))
  (dolist (token (items (parent node)))
    (if (perform-join-tests (tests node) token wme)
	(activate-children
	 node (make-instance 'token :parent token :wme wme)))))

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
