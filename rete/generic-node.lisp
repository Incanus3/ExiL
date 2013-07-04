(in-package :exil-rete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RETE is the algorithm that matches the facts in the knowledge base of an
;; expert system against conditions of its productions (inferences rules).
;; Without this algorithm, matching of each rule against each set of facts
;; would have unfeasible computational complexity even for medium-sized expert
;; systems.
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

;; this is needed in order to compare lists of exil objects
;; all these lists should be sets according to exil-equal-p, as they're all
;; updated using pusnew :test #'exil-equal-p, or similar
(defmethod exil-equal-p ((list1 list) (list2 list))
  (every (lambda (object) (member object list2 :test #'exil-equal-p)) list1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass node () ((children :accessor children :initform ())))

(defgeneric add-child (node child))
;; for top node, called by add-wme, for others, called by their parents
(defgeneric activate (node object)
  (:documentation "handles various node activations"))
(defgeneric activate-children (node object))
;; for top node, called by remove-wme for others by their parents
(defgeneric inactivate (node object))
(defgeneric inactivate-children (node object))

(defmethod add-child ((node node) (child node))
  (pushnew child (children node) :test #'node-equal-p)
  node)

(defvar *debug-rete* nil)

;; DEBUG:
(defmethod activate :before (node object)
  (when *debug-rete*
    (format t "~%~a~%  activated by ~a" node object)))

(defmethod inactivate :before (node object)
  (when *debug-rete*
    (format t "~%~a~%  inactivated by ~a" node object)))

;; TODO: check if the graph defined by rete nodes and their children is a tree
;; if it is, than the commented variant is probably just too memory-expansive
;; if it has cycles, than come up with some other good method to compare
;; the nodes according to the children
(defgeneric node-equal-p (node1 node2)
  (:method ((node1 node) (node2 node)) nil))

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

(defgeneric add-item (memory-node item &optional test)
  (:documentation "adds new item to node's memory"))
(defgeneric ext-add-item (memory-node item &optional test)
  (:documentation "adds new item to node's memory, as a second value returns
                   true if the item wasn't there yet"))
(defgeneric delete-item (memory-node item &optional test)
  (:documentation "removes item from node's memory"))

(defmethod add-item ((node memory-node) item &optional (test #'exil-equal-p))
  (pushnew item (items node) :test test))

(defmethod ext-add-item ((node memory-node) item &optional (test #'exil-equal-p))
  "pushes item to node's items, if it isn't already there, returns true if items were altered"
  (nth-value 1 (ext-pushnew item (items node) :test test)))

(defmethod delete-item ((node memory-node) item &optional (test #'exil-equal-p))
  (setf (items node) (delete item (items node) :test test)))
