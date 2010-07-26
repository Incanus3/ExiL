(in-package :exil)

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

;; described-object for debugging purposes

(defclass described-object () ((description :initarg :description
					    :initform ""
					    :accessor description)))

(defmethod print-object :after ((object described-object) stream)
  (unless (equal (description object) "")
    (format stream "  ~A" (description object))))

;; YOU SHOULD REMOVE THE DESCRIBED-OBJECT SUPERCLASS AFTER PROPER DEBUG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; generic node classes

(defclass node (described-object) ((children :accessor children :initform ())))

(defgeneric node-equal-p (node1 node2)
  (:method ((node1 (eql nil)) (node2 (eql nil))) t)
  (:method ((node1 node) (node2 node)) (equalp node1 node2)))

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


