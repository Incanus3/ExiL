(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun substitute-vars-in-goals (env bindings)
  (setf (goals env)
	(mapcar (lambda (goal) (substitute-variables goal bindings))
		(goals env))))

(defun select-match (matches)
  (first matches))

(defvar *print-match* t)

(defmacro add-object-to-tried-list (object tried-facts tried-rules)
  (let ((object-sym (gensym "object")))
    `(let ((,object-sym ,object))
       (etypecase ,object
         (fact (push ,object ,tried-facts))
         (rule (push ,object ,tried-rules))))))

(defun stack-match-for-backtrack (env match tried-facts tried-rules)
  (add-object-to-tried-list (goal-match-object match) tried-facts tried-rules)
  (stack-for-backtrack env (copy-list (goals env)) tried-facts tried-rules match))

(defun add-rule-conditions-to-goals (env goal-match-object)
  (when (rulep goal-match-object)
    (dolist (condition (conditions goal-match-object))
      (add-goal env condition))))

(defun make-back-step (env match &optional tried-facts tried-rules)
  (when *print-match* (print-goal-match match))
  (stack-match-for-backtrack env match tried-facts tried-rules)
  (del-goal env (goal-match-goal match))
  (add-rule-conditions-to-goals env (goal-match-object match))
  (substitute-vars-in-goals env (goal-match-bindings match))
  t)

(defun backtrack (env)
  ;; WHEN WE RUN OUT OF ALTERNATIVES, THE STACK SHOULD BE EMPTY
  (iter (while (back-stack env))
	(pop-backtrack (goals tried-facts tried-rules) env
	  (let ((matches (find-unused-matches
			  env (select-goal goals) tried-facts tried-rules)))
            ;; WHEN NO MATCHES, LOOP CONTINUES
	    (when matches
	      (setf (goals env) goals)
	      (return (make-back-step
		       env (select-match matches) tried-facts tried-rules)))))))

;; public
;; TODO: this should be undoable
;; RELEVANT STATE: goals, back-stack
;; if there are no more goals, back-step recognizes this as final success
;; and returns nil, which is how back-run knows matches were found

;; possible states:
;; no more goals - answer was found (but may not be the last one)
;; goals present because only partial match was found in last step
;; goals present because there are no more matches
;; => back-run can't test only goals to know whether to continue
(defmethod back-step ((env environment) &optional (undo-label "(back-step)"))
  "make one inference step using backward chaining"
  (declare (ignore undo-label))
  ;; IF NO GOALS, SEE IF WE CAN BACKTRACK
  (if (goals env)
      (let ((matches (find-matches env (select-goal (goals env)))))
	(if matches
            ;; SEVERAL MATCHES CAN BE FOUND IN ONE STEP, THESE SHOULD BE ITERATED
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
  (fresh-format t "~%All goals have been satisfied")
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
