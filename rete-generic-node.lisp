(in-package :exil-rete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RETE is the algorithm that matches the facts in the knowledge base of an
;; expert system against conditions of its inferences rules. Without this
;; algorithm, matching of each rule against each set of facts would have
;; exponential computational complexity 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; terminology note:
;; be sure about distiction of following expressions
;; a) template-facts - instances of class template-fact
;;      - they're just facts with named slots
;;      - e.g. (car :color red :mph 160)
;; b) templates - instances of class template
;;      - prescriptions for template-facts - describe the slot names,
;;        default values, etc.
;;      - you can't create template-fact using make-fact without having
;;        a template for it
;; c) patterns - could be either in simple-object or in template-object form
;;      - they're not facts, they can include wildcards, variables, etc.
;;      - e.g. (on red-box ?some-other-box) ; simple-pattern
;;      - or (car :color ?some-color)       ; template-pattern

;; serves for debugging purposes, so I can attach a description to various
;; nodes of the network and then see them in the printed tree

(defclass described-object () ((description :initarg :description
                                            :initform ""
                                            :accessor description)))

(defmethod print-object :after ((object described-object) stream)
  (unless (equal (description object) "")
    (format stream "  ~A" (description object))))

;; this is needed in order to compare lists of exil objects
;; all these lists should be sets according to exil-equal-p, as they're all
;; updated using pusnew :test #'exil-equal-p, or similar

(defmethod exil-equal-p and ((list1 list) (list2 list))
  (every (lambda (object) (member object list2 :test #'exil-equal-p)) list1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: remove the described-object superclass after proper debug
(defclass node (described-object) ((children :accessor children :initform ())))

(defgeneric add-child (node child))
;; for top node, called by add-wme, for others, called by their parents
(defgeneric activate (node object)
  (:documentation "handles various node activations"))
(defgeneric activate-children (node object))
;; for top node, called by remove-wme
(defgeneric inactivate (node object))
(defgeneric inactivate-children (node object))

;; TODO: check if the graph defined by rete nodes and their children is a tree
;; if it is, than the commented variant is probably just too memory-expansive
;; if it has cycles, than come up with some other good method to compare
;; the nodes according to the children
(defmethod exil-equal-p and ((node1 node) (node2 node))
  (equalp node1 node2)
;  (exil-equal-p (children node1) (children node2))
  )

(defmethod add-child ((node node) (child node))
  (pushnew child (children node) :test #'exil-equal-p)
  node)

;; probably redundant, there may be no need to add more then one child at a time
;; as prophetized, not in use
;; (defmethod add-children ((node node) (children list))
;;   (dolist (child children node)
;;     (add-child node child)))

(defmethod activate-children ((node node) object)
  (dolist (child (children node))
    (activate child object)))

(defmethod inactivate-children ((node node) object)
  (dolist (child (children node))
    (inactivate child object)))

(defmethod inactivate ((node node) object)
  (inactivate-children node object))

;; every node that stores some info is subclass (usualy indirect) of memory-node
(defclass memory-node (node) ((items :accessor items :initform ())))

(defmethod add-item ((node memory-node) item &optional (test #'exil-equal-p))
  (ext-pushnew item (items node) :test test))
