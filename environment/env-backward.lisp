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

(defun make-back-step (env match &optional tried-facts)
  (print-match match)
  (stack-for-backtrack env (copy-list (goals env)) (cons (match-fact match) tried-facts))
  (del-goal env (match-goal match))
  (substitute-vars-in-goals env (match-bindings match)))

(defun backtrack (env)
  (iter (pop-backtrack (goals tried-facts) env
	  (let ((matches (list-difference
			  (find-matching-facts env (select-goal goals))
			  tried-facts
			  :test (lambda (match fact)
				  (exil-equal-p (match-fact match) fact)))))
	    (when matches
	      (setf (goals env) goals)
	      (make-back-step env (select-match matches) tried-facts)
	      (return))))))

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
      (fresh-format t "There are no goals")))
