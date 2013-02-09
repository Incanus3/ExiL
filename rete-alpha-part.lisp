(in-package :exil-rete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The alpha part of the rete network peforms matching of WMEs against
;; individual conditions.
;; It consists of top-node, subtop-nodes (one for each template + one for simple
;; facts), test-nodes and memory-nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass alpha-node (node) ())

;; slot dataflow-networks holds hash table of network top nodes in alpha memory.
;; for each template there is a dataflow network (accessible through
;; its template name) and one network is for simple-facts
;; slot simple-fact-key holds dataflow-networks hash-key for simeple-facts
;; if there was some constant name for this, it wouldn't be possible to create
;; template of such name
(defclass alpha-top-node (alpha-node)
  ((dataflow-networks :accessor networks :initform (make-hash-table))
   (simple-fact-key :reader simple-fact-key
                    :initform (gensym "simple-fact")))
  (:documentation "Top node of the alpha-part of rete network, stores one
                   dataflow network for each template and one for simple-facts"))

(defgeneric network (node &optional tmpl-name)
  (:documentation "returns dataflow network for given template name
                   or simple-fact network if template name omitted"))
(defgeneric (setf network) (value node &optional tmpl-name)
  (:documentation "sets dataflow network for given template name
                   or simple-fact network by default"))
(defgeneric initialize-network (node &optional tmpl-name)
  (:documentation "creates new dataflow network for given template name
                   or simple-fact network by default"))
(defgeneric ensure-network (node &optional tmpl-name)
  (:documentation "either returns dataflow network for given template name
                   (or simple-fact network by default) if it exists or
                   initializes a new one and returns it"))
(defgeneric network-key (node wme)
  (:documentation "returns dataflow-networks hash-key for given wme
                   i.e. either its tmpl-name or simple-fact-key"))

(defmethod network ((node alpha-top-node)
                    &optional (tmpl-name (simple-fact-key node)))
  (gethash tmpl-name (networks node)))

(defmethod (setf network) (value (node alpha-top-node)
                           &optional (tmpl-name (simple-fact-key node)))
  (setf (gethash tmpl-name (networks node)) value))

(defmethod initialize-network ((node alpha-top-node)
                               &optional (tmpl-name (simple-fact-key node)))
  (setf (network node tmpl-name)
        (make-instance 'alpha-subtop-node :tmpl-name tmpl-name)))

; called by rete-net-creation
(defmethod ensure-network ((node alpha-top-node)
                           &optional (tmpl-name (simple-fact-key node)))
  (or (network node tmpl-name)
      (initialize-network node tmpl-name)))

(defmethod network-key ((node alpha-top-node) (wme fact))
  (typecase wme
    (simple-fact (simple-fact-key node))
    (template-fact (tmpl-name wme))))

;; called by add-wme
;; activates appropriate subtop node
(defmethod activate ((node alpha-top-node) (wme fact))
  (activate (network node (network-key node wme)) wme))

;; called by remove-wme
;; inactivates appropriate subtop node
(defmethod inactivate ((node alpha-top-node) (wme fact))
  (inactivate (network node (network-key node wme)) wme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; after alpha-top-node selects the right dataflow network according to fact type
;; and (in case of template-fact) template name, it activates the appropriate
;; subtop node
;; the subtop-nodes are created, when condition of that template (or simple)
;; appears for the first time in some newly added rule
(defclass alpha-subtop-node (alpha-node)
  ;; stored for debug purposes
  ((tmpl-name :reader tmpl-name :initarg :tmpl-name)))

(defmethod print-object ((node alpha-subtop-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "| tmpl-name: ~A" (tmpl-name node))))

(defmethod activate ((node alpha-subtop-node) (wme fact))
  (activate-children node wme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; performs constant test on the with which it's activated by comparing the
;; tested-field to the desired-value, when the test is successful, activates
;; its children and the alpha-memory, that's attached (if any)
;; TODO: check if it makes sense to differentiate between memory and child
;; activation, if not, memory could be just another child
(defclass alpha-test-node (alpha-node)
  ((tested-field :reader tested-field :initarg :tested-field
                 :initform (error "tested-field slot has to be specified"))
   (desired-value :reader value :initarg :value
                  :initform (error "desired-value slot has to be specified"))
   (alpha-memory :accessor memory :initarg :memory
                 :initform nil)))

(defgeneric test (node wme)
  (:documentation "provides testing part of alpha-test-node activation"))

(defmethod exil-equal-p and ((node1 alpha-test-node)
                             (node2 alpha-test-node))
  (and (equalp (tested-field node1)
               (tested-field node2))
       (constant-test (value node1)
                      (value node2))))

(defmethod print-object ((node alpha-test-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "| field: ~A, value: ~A" (tested-field node) (value node))))

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

;; inactivate standard method inactivates children
(defmethod inactivate :after ((node alpha-test-node) (wme fact))
  (when (memory node)
    (inactivate (memory node) wme)))

(defmethod test ((node alpha-test-node) (wme fact))
  (constant-test (value node) (object-slot wme (tested-field node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; children are beta-join-nodes
;; stores wmes that passed the constant tests for one condition of some rule
(defclass alpha-memory-node (alpha-node memory-node)
  ;; stored for debug purposes
  ((pattern :reader pattern :initarg :pattern)))

(defmethod print-object ((node alpha-memory-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "| pattern: ~A" (pattern node))))

(defmethod activate ((node alpha-memory-node) (wme fact))
  (add-item node wme)
  (activate-children node wme))

(defmethod inactivate ((node alpha-memory-node) (wme fact))
  (delete-item node wme)
  (inactivate-children node wme))
