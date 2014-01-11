(in-package :exil-rete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; private methods

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

;; THIS RETURNS ONLY ONE OCCURRENCE - is that a problem?
;; THIS SKIPS NEGATIVE CONDITIONS, which is a dirty hack, but the fastest way
;; returns (cons <condition index from end> <atom-position in condition>)
(defmethod find-atom-in-cond-list (atom cond-list)
  (or (iter (for condition :in (reverse cond-list))
            (for i :upfrom 1)
            ;; (when (atom-position condition atom)
            (when (and (not (negated-p condition)) ;; skip negative conds
                       (atom-position condition atom))
              (return (cons i (atom-position condition atom)))))
      ;; return pair even if not found
      (cons nil nil)))


;; TODO: get-intercondition-tests and get-intracondition-tests shouldn't
;; make tests for the singleton variable '?

;; ONLY THIS METHOD SHOULD KNOW TO SKIP PREVIOUS NEGATIVE CONDITIONS, BUT SINCE
;; FINDING ATOM IN PREVIOUS CONDITIONS IS HANDLED BY find-atom-in-cond-list
;; I'LL HACK IT THERE
;; get list of tests ensuring consistent variable bindings between
;; simple-pattern condition and prev-conds
(defmethod get-intercondition-tests ((condition simple-pattern)
                                     (prev-conds list))
  ;; iterate over atoms of current condition
  (iter (for atom :in (pattern condition))
        (with used-vars)
        (for i :upfrom 0)
        ;; if it's a variable
        (when (variable-p atom)
          ;; find the atom in previous conditions
          ;;                # conds back . which field
          (destructuring-bind (prev-cond . field)
              (find-atom-in-cond-list atom prev-conds)
            ;; if it occurrs in previous conditions and it wasn't checked yet
            ;; - this prevents checking multiple occurences of var in current
            ;;   cond against previous conds
            (when (and prev-cond (not (member atom used-vars)))
              ;; create a test for it
              (collect (make-test i prev-cond field))
              ;; mark the variable as checked
              (push atom used-vars))))))

;; get list of tests ensuring consistent variable bindings between
;; template-pattern condition and prev-conds
(defmethod get-intercondition-tests ((condition template-pattern)
                                     (prev-conds list))
  (iter (for (slot-name . slot-val) :in (slots condition))
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
  (iter (for subpattern :on (pattern condition))
        (for i :upfrom 0)
        (for other-occurrence = (position (first subpattern) (rest subpattern)))
        (when (and (variable-p (first subpattern)) other-occurrence)
          (collect (make-test 0 i (+ 1 i other-occurrence))))))

;; get list of tests ensuring consistent variable bindings within
;; template-pattern condition
(defmethod get-intracondition-tests ((condition template-pattern))
  (iter (for subpattern :on (slots condition))
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
  ;; child lists are sets w.r.t. node-equal-p, so this won't add existing node
  (add-child parent test-node)
  (add-child alpha-mem test-node)
  test-node)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; public methods

;; iterate over production's conditions creating missing rete-nework nodes
;; finally add the production to bottom beta-memory-node's productions
(defmethod new-production ((rete rete) (production rule))
  (with-slots (conditions) production
    (iter (for current-cond :in conditions)
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
