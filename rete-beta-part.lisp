(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beta memory classes

(defclass beta-node (node) ((parent :accessor parent :initarg :parent
				    :initform nil)))

;; could be changed to cons to be more effective
(defclass token () ((parent :reader parent :initarg :parent :initform nil)
		    (wme :reader wme :initarg :wme
			 :initform (error "wme slot has to be specified"))))

(defclass empty-token (token) ((wme :initform nil)))

(defmethod token (wme &optional parent)
  (make-instance 'token :wme wme :parent parent))

(defmethod previous-wme ((token token) &optional (n 1))
  "gives wme from token n wmes back"
  (dotimes (i n (wme token))
    (setf token (parent token))))

;; it would be more logical if the arguements were switched, but this is
;; more convenient, when i supply this predicate as a test to delete
(defmethod includes-p ((fact fact) (token token))
  (loop for tkn = token then (parent tkn)
     unless tkn do (return nil)
     when (fact-equal-p fact (wme tkn)) do
       (return t)))

(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream :type t)
    (format stream "~A"
	    (nreverse
	     (loop for tkn = token then (parent tkn)
		while tkn
		collect (wme tkn))))))

(defgeneric token-equal-p (token1 token2)
  (:documentation "token equality predicate")
  (:method (token1 token2) nil)
  (:method ((token1 empty-token) (token2 empty-token)) t)
  (:method ((token1 token) (token2 token))
    (and (fact-equal-p (wme token1) (wme token2))
	 (token-equal-p (parent token1) (parent token2)))))

;; children are beta-join-nodes
(defclass beta-memory-node (beta-node memory-node)
  ((productions :accessor productions
;		:initarg :productions
		:initform ())))

;; forward declarations, real ones will appear in environment.lisp
(defgeneric agenda (&optional environment))
(defgeneric add-match (match &optional agenda))
(defgeneric remove-match (match &optional agenda))

(defmethod complete-match ((node beta-memory-node) (token token))
  (dolist (production (productions node))
    (add-match (agenda) (cons production token))))

(defmethod activate ((node beta-memory-node) (token token))
  (when (nth-value 1 (pushnew token (items node) :test #'token-equal-p))
    (complete-match node token))
  (activate-children node token))

(defmethod broken-match ((node beta-memory-node) (token token))
  (dolist (production (productions node))
    (remove-match (agenda) (cons production token))))

;; SPATNE - varianta delete musi jako druhou hodnotu vratit list
;; tokenu, ktere smazala, ty se musi poslat funkci broken match
(defmethod inactivate ((node beta-memory-node) (fact fact))
  (multiple-value-bind (new-items deleted) 
      (diff-delete fact (items node) :test #'includes-p)
    (setf (items node) new-items)
    (dolist (item deleted)
      (broken-match node item))))

(defmethod add-production ((node beta-memory-node) (production rule))
  (push-update production (productions node) :test #'rule-equal-p))

(defmethod delete-production ((node beta-memory-node) (production rule))
  (setf (productions node)
	(delete production (productions node) :test #'rule-equal-p)))

(defmethod print-object ((node beta-memory-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "productions: ~A" (productions node))))

(defclass beta-top-node (beta-memory-node) 
  ((items :initform (list (make-instance 'empty-token)))))

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

(defun make-test (current-field previous-condition previous-field)
  (make-instance 'test
		 :current-field current-field
		 :previous-condition previous-condition
		 :previous-field previous-field))

(defmethod print-object ((test test) stream)
  (print-unreadable-object (test stream :type t :identity t)
    (with-slots (current-field-to-test previous-condition-number previous-field-to-test)
	test
      (format stream "~A" (list current-field-to-test previous-condition-number
				previous-field-to-test)))))

(defmethod test-equal-p ((test1 test) (test2 test))
  (with-accessors ((cf1 current-field) (pc1 previous-condition) (pf1 previous-field))
      test1
    (with-accessors ((cf2 current-field) (pc2 previous-condition) (pf2 previous-field))
	test2
      (and (equalp cf1 cf2) (= pc1 pc2) (equalp pf1 pf2)))))

(defmethod tests-equal-p ((test-list1 list) (test-list2 list))
  (and (= (length test-list1) (length test-list2))
       (every #'test-equal-p test-list1 test-list2)))

;; children are beta-memory-nodes
;; there's always just one child
(defclass beta-join-node (beta-node)
  ((alpha-memory :reader alpha-memory :initarg :alpha-memory
		 :initform (error "alpha-memory slot has to be specified"))
   (tests :accessor tests :initarg :tests :initform ())))

(defmethod initialize-instance :after ((node beta-join-node)
				       &key (beta-memory
					     (make-instance 'beta-memory-node
							    :parent node)))
  (when (equalp beta-memory 'production)
    (setf beta-memory (make-instance 'production-node :parent node)))
  (add-child node beta-memory))

(defmethod beta-memory ((node beta-join-node))
  (first (children node)))

(defmethod perform-join-test ((test test) (token token) (wme fact))
  (atom-equal-p (fact-field wme (current-field test))
		(fact-field (previous-wme token (previous-condition test))
			    (previous-field test))))

(defmethod perform-join-tests ((tests list) (token token) (wme fact))
  (dolist (test tests t)
    (unless (perform-join-test test token wme) (return nil))))
    
;; left activation
(defmethod activate ((node beta-join-node) (token token))
  (dolist (wme (items (alpha-memory node)))
    (if (perform-join-tests (tests node) token wme)
	(activate-children
	 node (make-instance 'token :parent token :wme wme)))))

;; right activation
(defmethod activate ((node beta-join-node) (wme fact))
  (dolist (token (items (parent node)))
    (if (perform-join-tests (tests node) token wme)
	(activate-children
	 node (make-instance 'token :parent token :wme wme)))))

(defmethod node-equal-p ((node1 beta-join-node)
			 (node2 beta-join-node))
  (with-slots ((am1 alpha-memory) (tsts1 tests) (par1 parent)) node1
    (with-slots ((am2 alpha-memory) (tsts2 tests) (par2 parent)) node2
      (and (node-equal-p am1 am2)
	   (node-equal-p par1 par2)
	   (tests-equal-p tsts1 tsts2)))))

#|
(defclass beta-top-node (beta-join-node) ())

(defmethod activate ((node beta-top-node) (wme fact))
  (activate-children node (make-instance 'token :parent nil :wme wme)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compound rete class and methods for export

(defclass rete () ((alpha-top-node :reader alpha-top-node
				   :initform (make-instance 'alpha-top-node))
		   (beta-top-node  :accessor beta-top-node
				   :initform (make-instance 'beta-top-node))))

(defmethod add-wme ((fact fact) &optional (rete (rete)))
  (activate (alpha-top-node rete) fact))

(defmethod remove-wme ((fact fact) &optional (rete (rete)))
  (inactivate (alpha-top-node rete) fact)
  (inactivate (beta-top-node rete) fact))
