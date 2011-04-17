(in-package :exil-rete)

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

(defgeneric activate (node object)
  (:documentation "handels various node activations"))

(defgeneric activate-children (node object)
  (:method ((node node) object)
    (dolist (child (children node))
      (activate child object))))

;; called by remove-wme
(defgeneric inactivate (node object))

(defmethod inactivate-children ((node node) object)
  (dolist (child (children node))
    (inactivate child object)))

(defmethod inactivate ((node node) object)
  (inactivate-children node object))

(defclass memory-node (node) ((items :accessor items :initform ())))

(defmethod add-item ((node memory-node) item &optional (equality-predicate #'equalp))
  (ext-pushnew item (items node) :test equality-predicate))