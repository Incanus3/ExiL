(in-package :exil-rete)

;; performs constant test on the with which it's activated by comparing the
;; tested-field to the desired-value, when the test is successful, activates
;; its children and the alpha-memory, that's attached (if any)
;; TODO: check if it makes sense to differentiate between memory and child
;; activation, if not, memory could be just another child
(defclass alpha-test-node (alpha-node)
  ((tested-field :reader tested-field :initarg :tested-field)
   (desired-value :reader value :initarg :value)
   (alpha-memory :accessor memory :initarg :memory
                 :initform nil)))

(defgeneric test (node wme)
  (:documentation "provides testing part of alpha-test-node activation"))
(defgeneric activate-memory (node wme))

(defmethod test ((node alpha-test-node) (wme fact))
  (constant-test (value node) (object-slot wme (tested-field node))))

(defmethod activate-memory ((node alpha-test-node) (wme fact))
  (with-slots ((mem alpha-memory)) node
    (when mem
      (activate mem wme))))

;; once the wme passes test of some of node's children, there's no need
;; to continue the search, because the children are created in a way
;; that no wme can pass 2 children's tests
;; TODO: stop the loop in this case - redefine activate-children
;; TODO: check if the net creation reuses parts of the alpha net, when
;; similar conditions are used (that differ e.g. from the third slot on)
;; if not, there's always only one child

;; returns test return value, thanks to this it is possible for
;; activate-children to break after first successful test
(defmethod activate ((node alpha-test-node) (wme fact))
  (let ((test (test node wme)))
    (when test
      (activate-children node wme)
      (activate-memory node wme))
    test))

(defmethod inactivate :after ((node alpha-test-node) (wme fact))
  (when (memory node)
    (inactivate (memory node) wme)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; children are beta-join-nodes
;; stores wmes that passed the constant tests for one condition of some rule
(defclass alpha-memory-node (alpha-node memory-node)
  ;; stored for debug purposes
  ((pattern :accessor pattern :initarg :pattern)))

(defmethod activate ((node alpha-memory-node) (wme fact))
  ;; this ensures that e.g. (palindrome ?p) doesn't match (palindrome a b a)
  (when (congruent (pattern node) wme)
    (add-item node wme)
    (activate-children node wme)))

(defmethod inactivate :before ((node alpha-memory-node) (wme fact))
  (delete-item node wme))
