(in-package :exil-env)

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

(defun original-variable-p (var env)
  (find var (variables-in-goals (original-goals env))))

(defun used-substitutions (env)
  (remove-if-not
   (lambda (subst) (original-variable-p (car subst) env))
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
