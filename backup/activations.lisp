(in-package :exil-env)

; private
(defmethod variable-bindings ((pattern simple-pattern) (fact simple-fact))
  (loop for atom in (pattern pattern)
     for i = 0 then (1+ i)
     when (variable-p atom)
       collect (cons atom (fact-slot fact i))))

; private
(defmethod variable-bindings ((pattern template-pattern) (fact template-fact))
  (loop for (slot-name . slot-val) in (slots pattern)
     when (variable-p slot-val)
       collect (cons slot-val (fact-slot fact slot-name))))

;; if the variable bindings of particular patter-fact pairs aren't consistent
;; resulting binding list will include more than one binding for that variable
; private
(defun get-variable-bindings (pattern-list fact-list)
  (cl:assert (= (length pattern-list)
		(length fact-list)) ()
    (string-append "get-variable-bindings: fact-list and patter-list"
                   "has to be of the same length"))
  (remove-duplicates (mapcan #'variable-bindings pattern-list fact-list) :test #'equalp))

; private
(defun substitute-variables (activations-with-vars var-bind-list)
  (let ((activations (copy-tree activations-with-vars)))
    (dolist (binding var-bind-list activations)
      (setf activations (subst (cdr binding) (car binding) activations)))))

; public
(defmethod activate-rule ((activation match))
  (let* ((rule (match-rule activation))
	 (token (match-token activation))
	 (bindings (get-variable-bindings (remove-if #'negated-p (conditions rule))
					  (token->list token)))
	 (activations (substitute-variables (activations rule)
					    bindings)))
    (format t "Firing ~A~%" activation)
    (dolist (activation activations)
      (eval activation))))