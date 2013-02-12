(in-package :exil-rete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compound rete class and methods for export

(defvar *debug-rete* nil)

;; rete stores reference to environment, to be able to call its add-match and
;; remove-match callbacks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass rete () ((alpha-top-node :reader alpha-top-node
                                     :initform (make-instance 'alpha-top-node))
                     (beta-top-node  :accessor beta-top-node
                                     :initform (make-instance 'beta-top-node))
                     (environment :reader environment :initarg :environment)))

  (defun make-rete (environment)
    (make-instance 'rete :environment environment)))

(defmethod add-wme ((rete rete) (wme fact))
  (when *debug-rete*
    (format t "~%------------------------------------------------------"))
  (activate (alpha-top-node rete) wme))

(defmethod rem-wme ((rete rete) (wme fact))
  (when *debug-rete*
    (format t "~%------------------------------------------------------"))
  (inactivate (alpha-top-node rete) wme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; net creation
;; parent can be either alpha-test-node or alpha-subtop-node
(defmethod find-test-node ((parent alpha-node) field value)
  (dolist (child (children parent) nil)
    (when (and (equalp (tested-field child) field)
               (var-or-equal-p (value child) value))
      (return child))))

(defmethod find/create-test-node ((parent alpha-node) field value)
  (let ((node (find-test-node parent field value)))
    (if node
        (values node nil)
        (values (make-instance 'alpha-test-node
                               :tested-field field
                               :value value) t))))

(defmethod create-alpha-net% ((pattern simple-pattern)
                              (root alpha-subtop-node))
  (iter (with patt = (pattern pattern))
        (with node = root)
        (for atom in patt)
        (for field :upfrom 0)
        (multiple-value-bind (child created-p)
            (find/create-test-node node field atom)
          (when created-p (add-child node child))
          (setf node child))
        (finally (return (if (memory node)
                             (memory node)
                             (setf (memory node)
                                   (make-instance 'alpha-memory-node
                                                  :pattern pattern)))))))

(defmethod create-alpha-net% ((pattern template-pattern)
                              (root alpha-subtop-node))
  (iter (with slots = (slots pattern))
        (with node = root)
        (for (slot-name . slot-value) in slots)
        (multiple-value-bind (child created-p)
            (find/create-test-node node slot-name slot-value)
          (when created-p (add-child node child))
          (setf node child))
        (finally (return (if (memory node)
                             (memory node)
                             (setf (memory node)
                                   (make-instance 'alpha-memory-node
                                                  :pattern pattern)))))))

(defmethod create-alpha-net ((rete rete) (pattern simple-pattern))
  (create-alpha-net% pattern (ensure-network (alpha-top-node rete))))

(defmethod create-alpha-net ((rete rete) (pattern template-pattern))
  (create-alpha-net% pattern (ensure-network (alpha-top-node rete)
                                             (tmpl-name pattern))))

(defun find-atom-in-cond-list% (atom cond-list)
  (iter (for condition in (reverse cond-list))
        (for i :upfrom 1)
        (when (find-atom condition atom)
          (return (cons i (atom-position condition atom))))))

(defmethod get-intercondition-tests% ((condition simple-pattern)
                                      (prev-conds list))
  (iter (for atom in (pattern condition))
        (with used-vars)
        (for i :upfrom 0)
        (for (prev-cond . field) = (find-atom-in-cond-list% atom prev-conds))
        (when (and (variable-p atom) prev-cond (not (member atom used-vars)))
          (collect (make-test i prev-cond field))
          (push atom used-vars))))

(defmethod get-intercondition-tests% ((condition template-pattern)
                                      (prev-conds list))
  (iter (for (slot-name . slot-val) in (slots condition))
        (with used-vars)
        (for (prev-cond . field) = (find-atom-in-cond-list% slot-val prev-conds))
        (when (and (variable-p slot-val)
                   prev-cond
                   (not (member slot-val used-vars)))
          (collect (make-test slot-name prev-cond field))
          (push slot-val used-vars))))

(defmethod get-intracondition-tests% ((condition simple-pattern))
  (iter (for subpattern on (pattern condition))
        (for i :upfrom 0)
        (when (and (variable-p (first subpattern))
                   (position (first subpattern) (rest subpattern)))
          (collect (make-test 0 i (+ 1 i (position (first subpattern)
                                                   (rest subpattern))))))))

(defmethod get-intracondition-tests% ((condition template-pattern))
  (iter (for subpattern on (slots condition))
        (for (slot-name . slot-val) = (first subpattern))
        (when (and (variable-p slot-val)
                   (find slot-val (rest subpattern) :key #'cdr))
          (collect (make-test 0 slot-name (car (find slot-val (rest subpattern)
                                                     :key #'cdr)))))))

(defmethod get-join-tests-from-condition ((condition pattern)
                                          (prev-conds list))
  ;; get join tests of condition against prev-conds
  (append (get-intercondition-tests% condition prev-conds)
          ;; get internal condition tests (same variable twice in condition)
          (get-intracondition-tests% condition)))

(defmethod find/create-join-node ((rete rete)
                                  (parent beta-memory-node)
                                  (tests list)
                                  (a-memory alpha-memory-node))
  (let ((join-node (make-instance 'beta-join-node
                                  :parent parent
                                  :tests tests
                                  :alpha-memory a-memory
                                  :rete rete)))
    (or (find-if (lambda (child) (exil-equal-p child join-node))
                 (children parent))
        (progn (push join-node (children parent))
               (push join-node (children a-memory))
               join-node))))

(defmethod find/create-neg-node ((parent beta-memory-node)
                                 (tests list)
                                 (a-memory alpha-memory-node))
  (let ((neg-node (make-instance 'beta-negative-node
                                 :parent parent
                                 :tests tests
                                 :alpha-memory a-memory)))
    (or (find-if (lambda (child) (exil-equal-p child neg-node))
                 (children parent))
        (progn (push neg-node (children parent))
               (push neg-node (children a-memory))
               neg-node))))

;; DODELAT NEGATIVE NODY
(defmethod new-production ((rete rete) (rule rule))
  (with-slots (conditions) rule
    (iter (for current-cond in conditions)
          (for i :upfrom 0)
          (for prev-conds :first () :then (subseq conditions 0 i))
          (for alpha-memory = (create-alpha-net rete current-cond))
          (for tests :first ()
               :then (get-join-tests-from-condition current-cond prev-conds))
          (for current-mem-node :first (beta-top-node rete)
               :then (beta-memory current-join-node))
          (for current-join-node = 
               (if (negated-p current-cond)
                   (find/create-neg-node current-mem-node tests
                                         alpha-memory)
                   (find/create-join-node rete current-mem-node tests
                                          alpha-memory)))
          (finally (add-production (beta-memory current-join-node) rule)))))
	 
(defmethod remove-production ((rete rete) (production rule))
  (labels ((walk-through (node)
             (when (typep node 'beta-memory-node)
               (delete-production node production))
             (dolist (child (children node))
               (walk-through child))))
    (walk-through (beta-top-node rete))))
