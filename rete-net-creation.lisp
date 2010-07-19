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
  (loop with pattern = (pattern pattern)
     with node = root
     for atom in pattern
     for field upto (1- (length pattern))
     do (multiple-value-bind (child created-p) (find/create-test-node node field atom)
;	  (print (list node child created-p))
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

(defmethod create-alpha-net ((rete rete) (pattern simple-pattern))
  (create-alpha-net% pattern (get/initialize-network (alpha-top-node rete))))

(defmethod create-alpha-net ((rete rete) (pattern template-pattern))
  (create-alpha-net% pattern (get/initialize-network (alpha-top-node rete)
					  (tmpl-name pattern))))

(defun  find-atom-in-cond-list% (atom cond-list)
  (loop for condition in (reverse cond-list)
     for i = 1 then (1+ i)
     until (find-atom atom condition)
     finally
       (let ((position (atom-postition atom condition)))
	 (when position (return (cons i position))))))

(defmethod get-join-tests-from-condition ((condition pattern)
					  (prev-conds list))
    (loop for atom in (pattern condition)
       for i = 0 then (1+ i)
       for (prev-cond . field) = (find-atom-in-cond-list% atom prev-conds)
	 then (find-atom-in-cond-list% atom prev-conds)
       when prev-cond
	 collect (make-test i prev-cond field)))
