(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pattern classes

(defclass pattern () ())

(defgeneric pattern-equal-p (pattern1 pattern2)
  (:method ((pattern1 pattern) (pattern2 pattern)) nil))

(defclass simple-pattern (pattern) ((fact :initarg :pattern :reader pattern)))

(defmacro make-pattern (pattern)
  `(make-instance 'simple-pattern :pattern ',pattern))

;; prints patterns
(defmethod print-object ((pattern simple-pattern) stream)
  (print-unreadable-object (pattern stream :type t)
    (format stream "~s" (pattern pattern))
    pattern))

;; checks pattern equivalency
(defmethod pattern-equal-p ((pattern1 simple-pattern) (pattern2 simple-pattern))
  (equalp (pattern pattern1) (pattern pattern2)))

;; checks pattern constant equivalency, ignoeres variables
(defmethod pattern-const-equal-p ((pattern1 simple-pattern)
				  (pattern2 simple-pattern))
  (every (lambda (atom1 atom2)
	   (or (and (variable-p atom1)
		    (variable-p atom2))
	       (equalp atom1 atom2)))
	 (pattern pattern1)
	 (pattern pattern2)))

(defclass template-pattern (pattern template) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fact matching

(defgeneric atom-equal-p (object1 object2)
  (:documentation "equality predicate for fact atoms")
  (:method (object1 object2) (equalp object1 object2)))

;; terminology note:
;; be sure about distiction of following 2 things
;; a) templates - instances of class template
;;      - they're just facts with named slots
;;      - e.g. (car (color red) (mph 160))
;; b) patterns - could be either in simple fact or in template fact form
;;      - they're not facts, they can include wildcards, variables, etc.
;;      - e.g. (on red-box ?some-other-box)
;;      - or (car (color ?some-color))

(defgeneric variable-bindings (pattern fact)
  (:documentation "if the fact passes the pattern,
                     returns the variable bindings in assoc list"))

(defun variable-p (symbol)
  (char-equal (char (symbol-name symbol) 0) #\?))

;; checks consistency of constant atoms in pattern, ignores variables
;; returns fact if passed, nil otherwise
(defmethod constant-check ((pattern simple-pattern)
			   (fact simple-fact))
  (when (every (lambda (pt-atom atom)
		 (or (variable-p pt-atom)
		     (equalp pt-atom atom)))
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
