(in-package :exil-rete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass beta-node (node) ((parent :accessor parent :initarg :parent
                                    :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beta-memory-node store (complete or partial) matches (sequences of wmes
;; that match some consecutive rule conditions)
;; botom beta-memory-nodes store productions (rules) that
;; are satisfied when the node is activated (i.e. the network path leading
;; to this node represents testing of all the production's conditions)
;; when these nodes are (in)activated, they signal add-match (remove-match)
;; to the environment, that's also why it stores reference to rete
;; (which stores reference to the environment)
;; children are beta-join-nodes, items are tokens

(defclass beta-memory-node (beta-node memory-node)
  ((productions :accessor productions
                ;; :initarg :productions
                :initform ())
   (rete :reader rete :initarg :rete
         :initform (error "rete has to be specified"))))

(defgeneric environment (node)
  (:documentation "environment in which this rete network is defined"))
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

(defmethod environment ((node beta-memory-node))
  (environment (rete node)))

;; when this is a new token, signal newly matched productions and activate
;; children
(defmethod activate ((node beta-memory-node) (token token))
  (when (nth-value 1 (ext-add-item node token #'token-equal-p))
    (dolist (production (productions node))
      (exil-env:add-match (environment node) production token))
    (activate-children node token)))

;; signal to environment that token (combined with any production)
;; is not a valid match any more
(defmethod broken-match ((node beta-memory-node) (token token))
  (dolist (production (productions node))
    (exil-env:remove-match (environment node) production token)))

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
  ((items :initform (list (make-empty-token)))
   (rete :initform nil)))

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
;; beta-join-node provides testing of variable binding consistency between
;; current rule condition (matching wmes stored in alpha memory feeding into
;; this node) and previous conditions (matchin tokens stored in beta memory
;; feeding into this node)
;; it can be activated either by the alpha memory when newly added wme matches
;; the current condition, or by the beta memory when newly added wme matches
;; some of the previous conditions, in both cases the other memory is searched
;; for items with consistent variable bindings
;; children are beta-memory-nodes, there's always just one child

(defclass beta-join-node (beta-node)
  ((alpha-memory :reader alpha-memory :initarg :alpha-memory
                 :initform (error "alpha-memory slot has to be specified"))
   (tests :accessor tests :initarg :tests :initform ())))

(defgeneric beta-memory (node)
  (:documentation "the beta memory node this node feeds into"))
(defgeneric perform-join-test (test token wme))
(defgeneric perform-join-tests (test-list token wme))

(defmethod initialize-instance :after ((node beta-join-node) &key rete)
  (add-child node (make-instance 'beta-memory-node :parent node :rete rete)))

(defmethod print-object ((node beta-join-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "| tests: ~A" (tests node))))

(defmethod beta-memory ((node beta-join-node))
  (first (children node)))

;; PERFORM JOIN TEST NEPODPORUJE INTRACONDITION TESTY - DODELAT
;; token doesn't include the wme, that's why there's 1- in the call
;; to previous-wme
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
        (activate-children node (make-token wme token)))))

;; right activation - by alpha-memory-node which feeds into it
(defmethod activate ((node beta-join-node) (wme fact))
  (dolist (token (items (parent node)))
    (if (perform-join-tests (tests node) token wme)
        (activate-children node (make-token wme token)))))

(defmethod exil-equal-p and ((node1 beta-join-node)
                             (node2 beta-join-node))
  (with-slots ((am1 alpha-memory) (tsts1 tests) (par1 parent)) node1
    (with-slots ((am2 alpha-memory) (tsts2 tests) (par2 parent)) node2
      (and ;(equalp (type-of node1) (type-of node2))
           (exil-equal-p am1 am2)
           (exil-equal-p par1 par2)
           (tests-equal-p tsts1 tsts2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beta-negative-node handles join tests (variable binding consistency tests
;; between rule's conditions) for negated conditions
;; these conditions are satisfied if there's no fact in the working memory
;; congruent with the pattern (with consistent variable bindings)

(defclass beta-negative-node (beta-join-node memory-node) ())

(defgeneric get-bad-wmes (node token))

;; returns list of wmes (from neg-node's alpha-memory), which have consistent
;; variable bindings with token, i.e. which pass the consistency tests
(defmethod get-consistent-wmes ((node beta-negative-node) (token token))
  (remove-if-not (lambda (wme) (perform-join-tests (tests node) token wme))
                 (items (alpha-memory node))))
    
;; left activation
;; when activated by token (previous conditions satisfied), ensure there are
;; no wmes consistent with the token, if not, activate children
;; store the consistent wmes (which are blocking the negated condition from
;; being satisfied) in the token for future use
(defmethod activate ((node beta-negative-node) (token token))
  (let ((bad-wmes (get-consistent-wmes node token)))
    (unless bad-wmes (activate-children node token))
    (setf (negative-wmes token) bad-wmes)
    (add-item node token #'token-equal-p)))

;; right activation
;; when activated by wme (from alpha memory) we need to check, whether this
;; breaks any negative condition match (if we find a token consistent with
;; this wme, where no consistent wmes were previously blocking
;; ((negative-wmes token) was empty), we need to signal this broken match
;; to children
(defmethod activate ((node beta-negative-node) (wme fact))
  (dolist (token (items node))
    (when (perform-join-tests (tests node) token wme)
      (unless (negative-wmes token)
        (inactivate-children node token))
      (pushnew wme (negative-wmes token) :test #'exil-equal-p))))

;; left inactivation only propagates to children

;; right inactivation
;; when inactivated by wme (from alpha memory) we need to check, wheter
;; there's some token previously blocked by this wme (i.e. this is the last
;; wme congruent with the negative condition, with bindings consistent with
;; the rest of token) and in that case signal newly satisfied negative
;; condition to children
(defmethod inactivate ((node beta-negative-node) (wme fact))
  (inactivate-children node wme)
  (dolist (token (items node))
    (when (and (negative-wmes token)
               (perform-join-tests (tests node) token wme))
      (setf (negative-wmes token)
            (delete wme (negative-wmes token) :test #'exil-equal-p))
      (unless (negative-wmes token)
        (activate-children node token)))))
