(in-package :exil)

(defmethod variable-bindings ((pattern simple-pattern) (fact simple-fact))
  (loop for atom in (pattern pattern)
     for i = 0 then (1+ i)
     when (variable-p atom)
       collect (cons atom (fact-slot fact i))))

(defmethod variable-bindings ((pattern template-pattern) (fact template-fact))
  (loop for (slot-name . slot-val) in (slots pattern)
     when (variable-p slot-val)
       collect (cons slot-val (fact-slot fact slot-name))))

;; if the variable bindings of particular patter-fact pairs aren't consistent
;; resulting binding list will include more than one binding for that variable
(defun get-variable-bindings (pattern-list fact-list)
  (cl:assert (= (length pattern-list)
		(length fact-list)) ()
    (string-append "get-variable-bindings: fact-list and patter-list"
                   "has to be of the same length"))
  (remove-duplicates (mapcan #'variable-bindings pattern-list fact-list) :test #'equalp))

(defun substitute-variables (tree var-bind-list)
  (let ((new-tree tree))
    (dolist (binding var-bind-list new-tree)
      (setf new-tree (subst (cdr binding) (car binding) new-tree)))))

(defmethod activate-rule ((activation match))
  (with-slots (rule token) activation
    (let* ((bindings (get-variable-bindings (remove-if #'negated (conditions rule))
					    (token->list token)))
	   (activations (substitute-variables (activations rule)
					      bindings)))
    (format t "Firing ~A~%" activation)
    (dolist (activation activations)
      (eval activation)))))