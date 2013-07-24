(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BACKWARD CHAINING

; public
(defmethod add-goal ((env environment) (goal pattern)
		     &optional (undo-label "(add-goal)"))
  (declare (ignore undo-label))
  (unless (find-goal env goal)
    (add-goal% env goal)))

; public
(defmethod print-goals ((env environment))
  (fresh-princ (goals env)))

(defun select-goal (goals)
  (first goals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-goal-match (goal fact/rule bindings)
  (list goal fact/rule bindings))

(defun goal-match-goal (match)
  (first match))

; match can hold either fact or rule
(defun goal-match-object (match)
  (second match))

(defun goal-match-fact (match)
  (goal-match-object match))

(defun goal-match-rule (match)
  (goal-match-object match))

(defun goal-match-bindings (match)
  (third match))

(defun print-goal-match (match)
  (fresh-format t "~A satisfied by ~A" (goal-match-goal match) (goal-match-fact match)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; returns list of goal-matches
(defun find-matching-facts (env goal)
  (iter (for fact :in (facts env))
	(multiple-value-bind (valid-match bindings)
	    (match-against-pattern fact goal)
	  (when valid-match
	    (collect (make-goal-match goal fact bindings))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; forward declaration
(defgeneric exil-parser:parse-pattern (env pattern-spec &key match-var))

(defun variables-in-goals (goals)
  (remove-duplicates (mapcan #'variables-in-pattern goals)))

(defun rule-rhs-assert-forms (rule)
  (remove-if-not (lambda (form)
		   (equalp (first form) 'assert))
		 (activations rule)))

(defun rule-rhs-assert-patterns (env rule)
  (mapcar (lambda (pattern-spec)
	    (exil-parser:parse-pattern env pattern-spec))
	  (mapcan #'rest (rule-rhs-assert-forms rule))))

(defun match-rule-against-goal (env rule goal)
  (let ((patterns (rule-rhs-assert-patterns env rule)))
    (multiple-value-bind (matching-pattern valid-match bindings)
	(find-if-func-result (lambda (pattern)
			       (match-against-pattern pattern goal))
			     patterns)
      (declare (ignore matching-pattern valid-match))
      (values t bindings))))

(defun find-matching-rules (env goal)
  (iter (for (name rule) :in-hashtable (rules env))
	(multiple-value-bind (valid-match bindings)
	    (match-rule-against-goal env rule goal)
	  (when valid-match (collect (make-goal-match goal rule bindings))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun substitute-vars-in-goals (env bindings)
  (setf (goals env)
	(mapcar (lambda (goal) (substitute-variables goal bindings))
		(goals env))))

(defun select-match (matches)
  (first matches))

(defvar *print-match* t)

(defun stack-match-for-backtrack (env match tried-facts tried-rules)
  (let ((goal-match-object (goal-match-object match)))
    (etypecase goal-match-object
      (fact (push goal-match-object tried-facts))
      (rule (push goal-match-object tried-rules)))
    (stack-for-backtrack env (copy-list (goals env))
			 tried-facts tried-rules match)))

(defun add-rule-conditions-to-goals (env rule)
  (dolist (condition (conditions rule))
    (add-goal env condition)))

(defun make-back-step (env match &optional tried-facts tried-rules)
  (when *print-match* (print-goal-match match))
  (stack-match-for-backtrack env match tried-facts tried-rules)
  (del-goal env (goal-match-goal match))
  (let ((goal-match-object (goal-match-object match)))
    (when (typep goal-match-object 'rule)
      (add-rule-conditions-to-goals env goal-match-object)))
  (substitute-vars-in-goals env (goal-match-bindings match))
  t)

(defun find-matches (env goal)
  (or (find-matching-facts env goal)
      (find-matching-rules env goal)))

(defun find-unused-fact-matches (env goal tried-facts)
  (list-difference (find-matching-facts env goal)
		   tried-facts
		   :test (lambda (match fact)
			   (exil-equal-p (goal-match-fact match) fact))))

(defun find-unused-rule-matches (env goal tried-rules)
  (list-difference (find-matching-rules env goal)
		   tried-rules
		   :test (lambda (match rule)
			   (rule-equal-p (goal-match-rule match) rule))))

(defun find-unused-matches (env goal tried-facts tried-rules)
  (or (find-unused-fact-matches env goal tried-facts)
      (find-unused-rule-matches env goal tried-rules)))

(defun backtrack (env)
  (iter (while (back-stack env))
	(pop-backtrack (goals tried-facts tried-rules) env
	  (let ((matches (find-unused-matches
			  env (select-goal goals) tried-facts tried-rules)))
	    (when matches
	      (setf (goals env) goals)
	      (return (make-back-step
		       env (select-match matches) tried-facts tried-rules)))))))

; public
(defmethod back-step ((env environment) &optional (undo-label "(back-step)"))
  "make one inference step using backward chaining"
  (declare (ignore undo-label))
  (if (goals env)
      (let* ((goal (select-goal (goals env)))
	     (matches (find-matches env goal)))
	(if matches
	    (make-back-step env (select-match matches))
	    (backtrack env)))
      (if *print-match* (fresh-format t "All goals have been satisfied"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compose-substs (subst1 subst2)
  (iter (with result = (copy-alist subst1))
	(for (var . val) :in subst2)
	(for (old-var . old-val) = (find var subst1 :key #'cdr))
	(if old-var
	    (setf (assoc-value old-var result) val)
	    (push-end (cons var val) result))
	(finally (return result))))

(defun compose-substitutions (substitutions)
  (remove-if (lambda (binding)
	       (equalp (car binding) (cdr binding)))
	     (reduce #'compose-substs substitutions)))

(defun original-goals (env)
  (first (last1 (back-stack env))))

(defun used-substitutions (env)
  (remove-if-not
   (lambda (subst) (find (car subst) (variables-in-goals (original-goals env))))
   (compose-substitutions
    (mapcar #'goal-match-bindings (back-stack-matches env)))))

(defun print-inference-report (env)
  (fresh-format t "All goals have been satisfied")
  (dolist (match (back-stack-matches env))
    (print-goal-match match))
  (let ((substitutions (used-substitutions env)))
    (fresh-format t "These variable bindings have been used:~%~A"
		  substitutions)
    substitutions))

(defmethod back-run ((env environment) &optional (undo-label "(back-run)"))
  (declare (ignore undo-label))
  (let (*print-match*)
    (iter (while (back-step env))))
  (if (goals env)
      (fresh-format t "No feasible answer found")
      (print-inference-report env)))
