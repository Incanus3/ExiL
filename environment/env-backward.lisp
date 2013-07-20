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
  (princ (goals env))
  nil)

(defun select-goal (goals)
  (first goals))

(defun make-goal-match (goal fact bindings)
  (list goal fact bindings))

(defun match-goal (match)
  (first match))

(defun match-fact (match)
  (second match))

(defun match-bindings (match)
  (third match))

(defun print-match (match)
  (fresh-format t "~A satisfied by ~A" (match-goal match) (match-fact match)))

; returns list of goal-matches
(defun find-matching-facts (env goal)
  (iter (for fact :in (facts env))
	(multiple-value-bind (valid-match bindings)
	    (match-fact-against-pattern fact goal)
	  (when valid-match
	    (collect (make-goal-match goal fact bindings))))))

(defun substitute-vars-in-goals (env bindings)
  (setf (goals env)
	(mapcar (lambda (goal) (substitute-variables goal bindings)) (goals env))))

(defun select-match (matches)
  (first matches))

(defvar *print-match* t)

(defun make-back-step (env match &optional tried-facts)
  (when *print-match* (print-match match))
  (stack-for-backtrack env (copy-list (goals env)) (cons (match-fact match) tried-facts) match)
  (del-goal env (match-goal match))
  (substitute-vars-in-goals env (match-bindings match))
  t)

(defun find-unused-matches (env goal tried-facts)
  (list-difference (find-matching-facts env goal)
		   tried-facts
		   :test (lambda (match fact)
			   (exil-equal-p (match-fact match) fact))))

(defun backtrack (env)
  (iter (while (back-stack env))
	(pop-backtrack (goals tried-facts) env
	  (let ((matches (find-unused-matches env (select-goal goals) tried-facts)))
	    (when matches
	      (setf (goals env) goals)
	      (return (make-back-step env (select-match matches) tried-facts)))))))

; public
(defmethod back-step ((env environment) &optional (undo-label "(back-step)"))
  "make one inference step using backward chaining"
  (declare (ignore undo-label))
  (if (goals env)
      (let* ((goal (select-goal (goals env)))
	     (matches (find-matching-facts env goal)))
	(if matches
	    (make-back-step env (select-match matches))
	    (backtrack env)))
      (if *print-match* (fresh-format t "All goals have been satisfied"))))

; temporary implementation
(defun compose-substitutions (substitutions)
  (remove-duplicates (apply #'append substitutions)))

(defun print-inference-report (env)
  (fresh-format t "All goals have been satisfied")
  (let ((matches (back-stack-matches env)))
    (dolist (match matches)
      (print-match match))
    (fresh-format t "These variable bindings have been used:~%~A"
		  (compose-substitutions (mapcar #'match-bindings matches)))))

(defmethod back-run ((env environment) &optional (undo-label "(back-run)"))
  (declare (ignore undo-label))
  (let (*print-match*)
    (iter (while (back-step env))))
  (if (goals env)
      (fresh-format t "No feasible answer found")
      (print-inference-report env)))
