(in-package :exil-rete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beta memory classes

(defclass beta-node (node) ((parent :accessor parent :initarg :parent
				    :initform nil)))

;; children are beta-join-nodes
(defclass beta-memory-node (beta-node memory-node)
  ((productions :accessor productions
;		:initarg :productions
		:initform ())))

;; forward declarations, real ones will appear in environment.lisp
;(defgeneric agenda ())
;(defgeneric add-match (rule token))
;(defgeneric remove-match (rule token))

(defmethod complete-match ((node beta-memory-node) (token token))
  (dolist (production (productions node))
    (exil-env:add-match production token)))

(defmethod activate ((node beta-memory-node) (token token))
;  (fresh-line t)
;  (format t "BETA-MEM-NODE ACTIVATED~%productions: ~A~%items before: ~A~%"
;	  (productions node) (items node))
  (when (nth-value 1 (add-item node token #'token-equal-p))
    (complete-match node token))
;  (format t "items after: ~A~%" (items node))
  (activate-children node token))

(defmethod broken-match ((node beta-memory-node) (token token))
  (dolist (production (productions node))
    (exil-env:remove-match production token)))

(defmethod inactivate :before ((node beta-memory-node) (fact fact))
  (multiple-value-bind (new-items deleted) 
      (diff-delete fact (items node) :test #'includes-p)
    (setf (items node) new-items)
    (dolist (item deleted)
      (broken-match node item))))

(defmethod inactivate :before ((node beta-memory-node) (token token))
  (multiple-value-bind (new-list deleted)
      (diff-delete token (items node) :test #'includes-p)
    (setf (items node) new-list)
    (dolist (item deleted)
      (broken-match node item))))

(defmethod add-production ((node beta-memory-node) (production rule))
  (push-update production (productions node) :test #'rule-equal-p)
  (dolist (item (items node))
    (complete-match node item)))

(defmethod delete-production ((node beta-memory-node) (production rule))
  (setf (productions node)
	(delete production (productions node) :test #'rule-equal-p)))

(defmethod print-object ((node beta-memory-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "productions: ~S" (productions node))))

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

;; PERFORM JOIN TEST NEPODPORUJE INTRACONDITION TESTY - DODELAT
(defmethod perform-join-test ((test test) (token token) (wme fact))
  (let ((previous-wme (previous-wme token (1- (previous-condition test)))))
    (when previous-wme
      (atom-equal-p (fact-slot wme (current-field test))
		    (fact-slot previous-wme (previous-field test))))))

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
      (and (equalp (type-of node1) (type-of node2))
	   (node-equal-p am1 am2)
	   (node-equal-p par1 par2)
	   (tests-equal-p tsts1 tsts2)))))

(defclass beta-negative-node (beta-join-node memory-node) ())

;; returns list of wmes (from neg-node's alpha-memory), which are
;; with given token var-consistent
(defmethod get-bad-wmes ((node beta-negative-node) (token token))
  (let (bad-wmes)
    (dolist (wme (items (alpha-memory node)) bad-wmes)
      (when (perform-join-tests (tests node) token wme)
	(push wme bad-wmes)))))
    
;; left activation
(defmethod activate ((node beta-negative-node) (token token))
  (let ((bad-wmes (get-bad-wmes node token)))
    (unless bad-wmes (activate-children node token))
    (setf (negative-wmes token) bad-wmes)
    (add-item node token #'token-equal-p))
  nil)

(defmethod activate ((node beta-negative-node) (wme fact))
  (dolist (token (items node))
    (when (perform-join-tests (tests node) token wme)
      (unless (negative-wmes token)
	(inactivate-children node token))
      (pushnew wme (negative-wmes token) :test #'fact-equal-p)))
  nil)

(defmethod inactivate ((node beta-negative-node) (wme fact))
  (inactivate-children node wme)
  (dolist (token (items node))
    (when (and (negative-wmes token)
	       (perform-join-tests (tests node) token wme))
      (setf (negative-wmes token)
	    (delete wme (negative-wmes token) :test #'fact-equal-p))
      (unless (negative-wmes token)
	(activate-children node token))))
  nil)