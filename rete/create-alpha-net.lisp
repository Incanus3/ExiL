(in-package :exil-rete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *debug-rete* nil)

;; rete stores reference to environment, to be able to call its add-match and
;; remove-match callbacks
(defclass rete () ((alpha-top-node :reader alpha-top-node
                                   :initform (make-instance 'alpha-top-node))
                   (beta-top-node  :accessor beta-top-node
                                   :initform (make-instance 'beta-top-node))
                   (environment :reader environment :initarg :environment)))

(defun make-rete (environment)
  (make-instance 'rete :environment environment))

;; public methods
(defgeneric add-wme (rete wme)
  (:documentation "called when new wme has been added to environment, may
                   result in add-match or remove-match callback"))
(defgeneric rem-wme (rete wme)
  (:documentation "called when wme has been removed from environment, may
                   result in add-match or remove-match callback"))
(defgeneric new-production (rete production)
  (:documentation "creates necessary rete network nodes for new production"))
(defgeneric remove-production (rete production)
  (:documentation "removes production from all beta memory nodes"))

(defmethod add-wme ((rete rete) (wme fact))
  (when *debug-rete*
    (format t "~%------------------------------------------------------"))
  (activate (alpha-top-node rete) wme))

(defmethod rem-wme ((rete rete) (wme fact))
  (when *debug-rete*
    (format t "~%------------------------------------------------------"))
  (inactivate (alpha-top-node rete) wme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; private methods
(defgeneric find-test-node (parent field value)
  (:documentation "finds test node with given tested-field and value
                   among parent's children"))
(defgeneric find/create-alpha-test-node (parent field value)
  (:documentation "either finds alpha-test-node with given tested-field and
                   value among parent's children or creates a new one"))
(defgeneric find/create-mem-node (parent pattern)
  (:documentation "either return parent's memory or create a new one for it"))
(defgeneric create-alpha-net% (pattern root)
  (:documentation "create alpha network for given pattern starting at root"))
(defgeneric create-alpha-net (rete pattern)
  (:documentation "first ensure existence of rete's alpha-subtop-node for
                   pattern's template, then create-alpha-net% for it"))

;; parent can be either alpha-test-node or alpha-subtop-node
(defmethod find-test-node ((parent alpha-node) field value)
  (find-if (lambda (child) (and (equalp (tested-field child) field)
                                (var-or-equal-p (value child) value)))
           (children parent)))

;; returns the found or created test-node and (as a second value), wherther
;; a new node has been created (i.e. wasn't found)
(defmethod find/create-alpha-test-node ((parent alpha-node) field value)
  (let ((node (find-test-node parent field value)))
    (if node
        (values node nil)
        (values (make-instance 'alpha-test-node
                               :tested-field field
                               :value value) t))))

(defmethod find/create-mem-node (parent pattern)
  (if (memory parent)
      (memory parent)
      (setf (memory parent)
            (make-instance 'alpha-memory-node
                           :pattern pattern))))

;; create alpha network (test-nodes and a memory node) for given simple-pattern
;; starting at the alpha-subtop-node and returning the bottom memory node
(defmethod create-alpha-net% ((pattern simple-pattern)
                              (root alpha-subtop-node))
  (iter (with patt = (pattern pattern))
        (with node = root)
        (for atom :in patt)
        (for field :upfrom 0)
        (multiple-value-bind (child created-p)
            (find/create-alpha-test-node node field atom)
          (when created-p (add-child node child))
          (setf node child))
        (finally (return (find/create-mem-node node pattern)))))

;; create alpha network (test-nodes and a memory node) for given template-pattern
;; starting at the alpha-subtop-node and returning the bottom memory node
(defmethod create-alpha-net% ((pattern template-pattern)
                              (root alpha-subtop-node))
  (iter (with slots = (slots pattern))
        (with node = root)
        (for (slot-name . slot-value) :in slots)
        (multiple-value-bind (child created-p)
            (find/create-alpha-test-node node slot-name slot-value)
          (when created-p (add-child node child))
          (setf node child))
        (finally (return (find/create-mem-node node pattern)))))

;; create alpha network in rete for given simple-pattern
(defmethod create-alpha-net ((rete rete) (pattern simple-pattern))
  (create-alpha-net% pattern (ensure-network (alpha-top-node rete))))

;; create alpha network in rete for given template-pattern
(defmethod create-alpha-net ((rete rete) (pattern template-pattern))
  (create-alpha-net% pattern (ensure-network (alpha-top-node rete)
                                             (name (template pattern)))))
