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

(defun select-goal (env)
  (first (goals env)))

(defun make-goal-match (goal fact bindings)
  (list goal fact bindings))

(defun match-goal (match)
  (first match))

(defun match-fact (match)
  (second match))

(defun match-bindings (match)
  (third match))

(defun print-match (match)
  (fresh-line)
  (format t "~A satisfied by ~A" (match-goal match) (match-fact match)))

(defun find-matching-fact (env goal)
  (dolist (fact (facts env))
    (multiple-value-bind (valid-match bindings)
	(match-fact-against-pattern fact goal)
      (when valid-match
	(return-from find-matching-fact
	  (make-goal-match goal fact bindings))))))

(defun substitute-vars-in-goals (env bindings)
  (setf (goals env)
	(mapcar (lambda (goal) (substitute-variables goal bindings)) (goals env))))

; public
(defmethod back-step ((env environment) &optional (undo-label "(back-step)"))
  (declare (ignore undo-label))
  "make one inference step using backward chaining"
  (let* ((goal (select-goal env))
	 (match (find-matching-fact env goal)))
    (when match
      (print-match match)
      (del-goal env goal)
      (substitute-vars-in-goals env (match-bindings match)))))
