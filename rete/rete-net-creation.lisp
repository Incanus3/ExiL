(in-package :exil-rete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compound rete class and methods for export

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
(defgeneric find-atom-in-cond-list (atom cond-list)
  (:documentation "which condition in cond-list includes atom and
                   in which position"))
(defgeneric get-intercondition-tests (condition prev-conds)
  (:documentation "get list of tests ensuring consistent variable bindings
                   between condition and prev-conds"))
(defgeneric get-intracondition-tests (condition)
  (:documentation "get list of tests ensuring consistent variable bindings
                   within condition"))
;; appends previous two
(defgeneric get-join-tests-from-condition (condition prev-conds)
  (:documentation "get list of tests ensuring consistent variable bindings
                   in condition and prev-conds"))
(defgeneric find/add-test-node (test-node parent alpha-mem)
  (:documentation "either find test-node in parent or add it to parent's
                   and alpha-mem's children"))
(defgeneric find/create-beta-test-node (rete node-type parent tests alpha-mem)
  (:documentation "find or create child beta-join- or beta-negative-node
                   for given beta-memory-node"))

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
        (for atom in patt)
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
        (for (slot-name . slot-value) in slots)
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
                                             (tmpl-name pattern))))

;; returns (cons <condition index from end> <atom-position in condition>)
(defmethod find-atom-in-cond-list (atom cond-list)
  (iter (for condition in (reverse cond-list))
        (for i :upfrom 1)
        (when (find-atom condition atom)
          (return (cons i (atom-position condition atom))))))

;; get list of tests ensuring consistent variable bindings between
;; simple-pattern condition and prev-conds
(defmethod get-intercondition-tests ((condition simple-pattern)
                                     (prev-conds list))
  (iter (for atom in (pattern condition))
        (with used-vars)
        (for i :upfrom 0)
        (for (prev-cond . field) = (find-atom-in-cond-list atom prev-conds))
        (when (and (variable-p atom) prev-cond (not (member atom used-vars)))
          (collect (make-test i prev-cond field))
          (push atom used-vars))))

;; get list of tests ensuring consistent variable bindings between
;; template-pattern condition and prev-conds
(defmethod get-intercondition-tests ((condition template-pattern)
                                     (prev-conds list))
  (iter (for (slot-name . slot-val) in (slots condition))
        (with used-vars)
        (for (prev-cond . field) = (find-atom-in-cond-list slot-val prev-conds))
        (when (and (variable-p slot-val)
                   prev-cond
                   (not (member slot-val used-vars)))
          (collect (make-test slot-name prev-cond field))
          (push slot-val used-vars))))

;; get list of tests ensuring consistent variable bindings within
;; simple-pattern condition (important when the same variable appears in the
;; condition more than once)
(defmethod get-intracondition-tests ((condition simple-pattern))
  (iter (for subpattern on (pattern condition))
        (for i :upfrom 0)
        (for other-occurrence = (position (first subpattern) (rest subpattern)))
        (when (and (variable-p (first subpattern)) other-occurrence)
          (collect (make-test 0 i (+ 1 i other-occurrence))))))

;; get list of tests ensuring consistent variable bindings within
;; template-pattern condition
(defmethod get-intracondition-tests ((condition template-pattern))
  (iter (for subpattern on (slots condition))
        (for (slot-name . slot-val) = (first subpattern))
        (for other-occurrence = (rassoc slot-val (rest subpattern)))
        (when (and (variable-p slot-val) other-occurrence)
          (collect (make-test 0 slot-name (car other-occurrence))))))

(defmethod get-join-tests-from-condition ((condition pattern)
                                          (prev-conds list))
  (append (get-intercondition-tests condition prev-conds)
          (get-intracondition-tests condition)))

(defmethod find/add-test-node ((test-node beta-join-node)
                               (parent beta-memory-node)
                               (alpha-mem alpha-memory-node))
  (or (find-if (lambda (child) (exil-equal-p child test-node))
               (children parent))
      (progn (add-child parent test-node)
             (add-child alpha-mem test-node)
             test-node)))

;; find or create child join- or negative-node for given beta-memory-node
;; node-type may be beta-join-node or beta-negative-node
(defmethod find/create-beta-test-node ((rete rete)
                                       (node-type symbol)
                                       (parent beta-memory-node)
                                       (tests list)
                                       (alpha-mem alpha-memory-node))
  (let ((test-node (make-instance node-type
                                  :parent parent
                                  :tests tests
                                  :alpha-memory alpha-mem
                                  :rete rete)))
    (find/add-test-node test-node parent alpha-mem)))

;; iterate over production's conditions creating missing rete-nework nodes
;; finally add the production to bottom beta-memory-node's productions
(defmethod new-production ((rete rete) (production rule))
  (with-slots (conditions) production
    (iter (for current-cond in conditions)
          (for i :upfrom 0)
          (for prev-conds :first () :then (subseq conditions 0 i))
          (for alpha-memory = (create-alpha-net rete current-cond))
          (for tests :first ()
               :then (get-join-tests-from-condition current-cond prev-conds))
          (for current-mem-node :first (beta-top-node rete)
               :then (beta-memory current-join-node))
          (for current-join-node = 
               (find/create-beta-test-node
                rete (if (negated-p current-cond)
                         'beta-negative-node 'beta-join-node)
                current-mem-node tests alpha-memory))
          (finally (add-production (beta-memory current-join-node) production)))))

;; remove production from all beta-memory-nodes' productions
(defmethod remove-production ((rete rete) (production rule))
  (labels ((walk-through (node)
             (when (typep node 'beta-memory-node)
               (delete-production node production))
             (dolist (child (children node))
               (walk-through child))))
    (walk-through (beta-top-node rete))))
