(in-package :exil)

;; parent can be either alpha-test-node or simple/pattern-fact-subtop-node
(defmethod find-test-node ((parent alpha-node) field value)
  (dolist (child (children parent) nil)
    (when (and (equalp (tested-field child) field)
	       (var-or-equal-p (value child) value))
      (return child))))

(defmethod find/create-test-node% (parent field value new-node-type)
  (let ((node (find-test-node parent field value)))
    (if node
	(values node nil)
	(values (make-instance new-node-type
			       :tested-field field
			       :value value) t))))

(defgeneric find/create-test-node (parent field value)
  (:method ((parent simple-fact-alpha-node) field value)
    (find/create-test-node% parent field value 'simple-fact-test-node))
  (:method ((parent template-fact-alpha-node) field value)
    (find/create-test-node% parent field value 'template-fact-test-node)))

(defmethod create-alpha-net% ((pattern simple-pattern) (root simple-fact-subtop-node))
  (loop with patt = (pattern pattern)
     with node = root
     for atom in patt
     for field = 0 then (1+ field)
     do (multiple-value-bind (child created-p) (find/create-test-node node field atom)
;	  (format t "atom: ~A~%field: ~A~%parent: ~A~%child: ~A~%created-p: ~A~%~%"
;		  atom field node child created-p)
	  (when created-p (add-child node child))
	  (setf node child))
     finally
;      (progn
;	 (print (list node (memory node)))
       (return (if (memory node)
		   (memory node)
		   (setf (memory node)
			 (make-instance 'alpha-memory-node))))))

(defmethod create-alpha-net% ((pattern template-pattern) (root template-fact-subtop-node))
  (loop with slots = (slots pattern)
     with node = root
     for (slot-name . slot-value) in slots
     do (multiple-value-bind (child created-p)
	    (find/create-test-node node slot-name slot-value)
	  (when created-p (add-child node child))
	  (setf node child))
     finally
       (return (if (memory node)
		   (memory node)
		   (setf (memory node)
			 (make-instance 'alpha-memory-node))))))

(defmethod create-alpha-net ((pattern simple-pattern) &optional (rete (rete)))
  (create-alpha-net% pattern (get/initialize-network (alpha-top-node rete))))

(defmethod create-alpha-net ((pattern template-pattern) &optional (rete (rete)))
  (create-alpha-net% pattern (get/initialize-network (alpha-top-node rete)
					  (tmpl-name pattern))))

(defun find-atom-in-cond-list% (atom cond-list)
  (loop for condition in (reverse cond-list)
     for i = 1 then (1+ i)
     until (find-atom atom condition)
     finally
       (let ((position (atom-postition atom condition)))
	 (when position (return (cons i position))))))

(defmethod get-intercondition-tests% ((condition simple-pattern) (prev-conds list))
  (loop for atom in (pattern condition)
     with used-vars
     for i = 0 then (1+ i)
     for (prev-cond . field) = (find-atom-in-cond-list% atom prev-conds)
     then (find-atom-in-cond-list% atom prev-conds)
     when (variable-p atom)
     unless (member atom used-vars)
     when prev-cond
     collect (make-test i prev-cond field)
     do (push atom used-vars)))

(defmethod get-intercondition-tests% ((condition template-pattern) (prev-conds list))
  (loop for (slot-name . slot-val) in (slots condition)
     with used-vars
     for (prev-cond . field) = (find-atom-in-cond-list% slot-val prev-conds)
     then (find-atom-in-cond-list% slot-val prev-conds)
     when (variable-p slot-val)
     unless (member slot-val used-vars)
     when prev-cond
     collect (make-test slot-name prev-cond field)
     do (push slot-val used-vars)))

(defmethod get-intracondition-tests% ((condition simple-pattern))
  (loop for subpattern on (pattern condition)
     for i = 0 then (1+ i)
     when (variable-p (first subpattern))
     when (position (first subpattern) (rest subpattern))
     collect (make-test 0 i (+ 1 i (position (first subpattern) (rest subpattern))))))

(defmethod get-intracondition-tests% ((condition template-pattern))
  (loop for subpattern on (slots condition)
     for (slot-name . slot-val) = (first subpattern) then
       (first subpattern)
     when (variable-p slot-val)
     when (find slot-val (rest subpattern) :key #'cdr)
       collect (make-test 0 slot-name (car (find slot-val (rest subpattern) :key #'cdr)))))

(defmethod get-join-tests-from-condition ((condition pattern)
					  (prev-conds list))
;; get join tests of condition against prev-conds
  (append (get-intercondition-tests% condition prev-conds)
;; get internal condition tests (same variable twice in condition)
	  (get-intracondition-tests% condition)))

(defmethod find/create-join-node ((parent beta-memory-node)
				  (tests list)
				  (a-memory alpha-memory-node))
  (let ((join-node (make-instance 'beta-join-node
				  :parent parent
				  :tests tests
				  :alpha-memory a-memory)))
    (or (find-if (lambda (child) (node-equal-p child join-node)) (children parent))
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
    (or (find-if (lambda (child) (node-equal-p child neg-node)) (children parent))
	(progn (push neg-node (children parent))
	       (push neg-node (children a-memory))
	       neg-node))))

;; DODELAT NEGATIVE NODY
(defmethod new-production ((rule rule) &optional (rete (rete)))
  (with-slots (conditions activations) rule
    (loop
       for current-cond in conditions
       for i = 0 then (1+ i)
       for prev-conds = () then (subseq conditions 0 i)
       for alpha-memory = (create-alpha-net current-cond rete) then
	 (create-alpha-net current-cond rete)
       for tests = () then
	 (get-join-tests-from-condition current-cond prev-conds)
       for current-mem-node = (beta-top-node rete) then
	 (beta-memory current-join-node)
       for current-join-node
	 = (if (negated current-cond)
	       (find/create-neg-node current-mem-node tests alpha-memory)
	       (find/create-join-node current-mem-node tests alpha-memory)) then
	 (if (negated current-cond)
	     (find/create-neg-node current-mem-node tests alpha-memory)
	     (find/create-join-node current-mem-node tests alpha-memory))
       finally
	 (add-production (beta-memory current-join-node)
			 rule))))
	 
(defmethod remove-production ((rule rule) &optional (rete (rete)))
  (labels ((walk-through (node)
	     (when (typep node 'beta-memory-node)
	       (delete-production node rule))
	     (dolist (child (children node))
	       (walk-through child))))
    (walk-through (beta-top-node rete))))
