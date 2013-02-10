(in-package :exil-rete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass beta-node (node) ((parent :accessor parent :initarg :parent
                                    :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; children are beta-join-nodes, items are tokens
;; botom memory nodes of beta rete network store productions (rules) that
;; are satisfied when the node is activated (i.e. the network path leading
;; to this node represents testing of all the production's conditions)
;; when these nodes are (in)activated, they signal add-match (remove-match)
;; to the environment
(defclass beta-memory-node (beta-node memory-node)
  ((productions :accessor productions
                ;; :initarg :productions
                :initform ())))

(defgeneric broken-match (node token)
  (:documentation "pairs token with each of node's productions and removes
                   this match from the environment"))
(defgeneric add-production (node production)
  (:documentation "adds production that is satisfied when node is activated"))
(defgeneric delete-production (node production)
  (:documentation "deletes production from productions list"))

(defmethod print-object ((node beta-memory-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "| productions: ~S" (productions node))))

;; when this is a new token, signal newly matched productions and activate
;; children
(defmethod activate ((node beta-memory-node) (token token))
  (when (nth-value 1 (ext-add-item node token #'token-equal-p))
    (dolist (production (productions node))
      (exil-env:add-match production token))
    (activate-children node token)))

;; signal to environment that token (combined with any production)
;; is not a valid match any more
(defmethod broken-match ((node beta-memory-node) (token token))
  (dolist (production (productions node))
    (exil-env:remove-match production token)))

;; left inactivation - the wme has been removed from working memory
;; remove tokens including it and signal broken matches
(defmethod inactivate :before ((node beta-memory-node) (wme fact))
  (multiple-value-bind (new-items deleted) 
      (diff-remove wme (items node) :test #'included-in-p)
    (setf (items node) new-items)
    (dolist (token deleted)
      (broken-match node token))))

;; right inactivation - some other wme, that matched some of the rule's
;; previous conditions, have been removed from working memory, so tokens
;; that include token are not valid matches any more
;; remove tokens including it and signal broken matches
(defmethod inactivate :before ((node beta-memory-node) (token token))
  (multiple-value-bind (new-list deleted)
      (diff-remove token (items node) :test #'included-in-p)
    (setf (items node) new-list)
    (dolist (item deleted)
      (broken-match node item))))

;; add production to productions and signal complete match for already
;; matched tokens
(defmethod add-production ((node beta-memory-node) (production rule))
  (push-update production (productions node) :test #'rule-equal-p)
  (dolist (token (items node))
    (exil-env:add-match production token)))

;; remove production from productions
;; exil-env:rem-rule, that calls this (indirectly), also removes matches from
;; agenda, so there's no need to signal broken matches
(defmethod delete-production ((node beta-memory-node) (production rule))
  (setf (productions node)
        (delete production (productions node) :test #'rule-equal-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dummy top-node
(defclass beta-top-node (beta-memory-node)
  ((items :initform (list (make-empty-token)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests holds info about one variable-binding consistency test
;; current- and previous-field-to-test are object-slot descriptors -
;; integer positions for simple-facts, slot names for template facts
(defclass test ()
  ((current-field-to-test
    :reader current-field :initarg :current-field
    :initform (error "current-field slot has to be specified"))
   (previous-condition-number
    :documentation "tells, how many conditions back i must go"
    :reader previous-condition :initarg :previous-condition
    :initform 0)
   (previous-field-to-test
    :reader previous-field :initarg :previous-field
    :initform (error "previous-field slot has to be specified"))))

(defgeneric test-equal-p (test1 test2))
(defgeneric tests-equal-p (test-list1 test-list2))

(defun make-test (current-field previous-condition previous-field)
  (make-instance 'test
                 :current-field current-field
                 :previous-condition previous-condition
                 :previous-field previous-field))

(defmethod print-object ((test test) stream)
  (with-slots (current-field-to-test previous-condition-number
                                     previous-field-to-test) test
    (if *print-escape*
        (print-unreadable-object (test stream :type t :identity nil)
          (format stream
                  "current-field: ~A, ~A conditions back, previous-field: ~A"
                  current-field-to-test previous-condition-number
                  previous-field-to-test))
        (format stream
                "(curr-field: ~A, ~A conds back, prev-field: ~A)"
                current-field-to-test previous-condition-number
                previous-field-to-test))))

(defmethod test-equal-p ((test1 test) (test2 test))
  (with-accessors ((cf1 current-field) (pc1 previous-condition)
                   (pf1 previous-field)) test1
    (with-accessors ((cf2 current-field) (pc2 previous-condition)
                     (pf2 previous-field)) test2
      (and (equalp cf1 cf2) (= pc1 pc2) (equalp pf1 pf2)))))

(defmethod tests-equal-p ((test-list1 list) (test-list2 list))
  (and (= (length test-list1) (length test-list2))
       (every #'test-equal-p test-list1 test-list2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; children are beta-memory-nodes
;; there's always just one child
(defclass beta-join-node (beta-node)
  ((alpha-memory :reader alpha-memory :initarg :alpha-memory
                 :initform (error "alpha-memory slot has to be specified"))
   (tests :accessor tests :initarg :tests :initform ())))

(defgeneric beta-memory (node)
  (:documentation "the beta memory node this node feeds into"))
(defgeneric perform-join-test (test token wme))
(defgeneric perform-join-tests (test-list token wme))

(defmethod initialize-instance :after ((node beta-join-node) &key)
  (add-child node (make-instance 'beta-memory-node :parent node)))

(defmethod print-object ((node beta-join-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "| tests: ~A" (tests node))))

(defmethod beta-memory ((node beta-join-node))
  (first (children node)))

;; PERFORM JOIN TEST NEPODPORUJE INTRACONDITION TESTY - DODELAT
(defmethod perform-join-test ((test test) (token token) (wme fact))
  (let ((previous-wme (previous-wme token (1- (previous-condition test)))))
    (when previous-wme
      (equalp (object-slot wme (current-field test))
              (object-slot previous-wme (previous-field test))))))

(defmethod perform-join-tests ((tests list) (token token) (wme fact))
  (dolist (test tests t)
    (unless (perform-join-test test token wme) (return nil))))
    
;; left activation - by parent beta-memory-node
(defmethod activate ((node beta-join-node) (token token))
  (dolist (wme (items (alpha-memory node)))
    (if (perform-join-tests (tests node) token wme)
        (activate-children
         node (make-instance 'token :parent token :wme wme)))))

;; right activation - by alpha-memory-node which feeds into it
(defmethod activate ((node beta-join-node) (wme fact))
  (dolist (token (items (parent node)))
    (if (perform-join-tests (tests node) token wme)
        (activate-children
         node (make-instance 'token :parent token :wme wme)))))

(defmethod exil-equal-p and ((node1 beta-join-node)
                             (node2 beta-join-node))
  (with-slots ((am1 alpha-memory) (tsts1 tests) (par1 parent)) node1
    (with-slots ((am2 alpha-memory) (tsts2 tests) (par2 parent)) node2
      (and ;(equalp (type-of node1) (type-of node2))
           (exil-equal-p am1 am2)
           (exil-equal-p par1 par2)
           (tests-equal-p tsts1 tsts2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass beta-negative-node (beta-join-node memory-node) ())

(defgeneric get-bad-wmes (node token))

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
      (pushnew wme (negative-wmes token) :test #'exil-equal-p)))
  nil)

(defmethod inactivate ((node beta-negative-node) (wme fact))
  (inactivate-children node wme)
  (dolist (token (items node))
    (when (and (negative-wmes token)
               (perform-join-tests (tests node) token wme))
      (setf (negative-wmes token)
            (delete wme (negative-wmes token) :test #'exil-equal-p))
      (unless (negative-wmes token)
        (activate-children node token))))
  nil)
