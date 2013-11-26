(in-package :exil-env)

(defmacro add-object-to-tried-list (object tried-facts tried-rules)
  (let ((object-sym (gensym "object")))
    `(let ((,object-sym ,object))
       (etypecase ,object
         (fact (push ,object-sym ,tried-facts))
         (rule (push ,object-sym ,tried-rules))))))

(defun stack-match-for-backtrack (env match tried-facts tried-rules)
  (add-object-to-tried-list (goal-match-object match) tried-facts tried-rules)
  (stack-for-backtrack env (copy-list (goals env)) tried-facts tried-rules match))

(defun add-rule-conditions-to-goals (env goal-match-object)
  (when (rulep goal-match-object)
    (dolist (condition (conditions goal-match-object))
      (add-goal env condition))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *print-match* t)

(defun make-back-step (env match undo-label &optional tried-facts tried-rules)
  (with-saved-slots env (goals back-stack) undo-label
    (when *print-match* (print-goal-match match))
    (stack-match-for-backtrack env match tried-facts tried-rules)
    (del-goal env (goal-match-goal match))
    (add-rule-conditions-to-goals env (goal-match-object match))
    (substitute-vars-in-goals env (goal-match-bindings match))
    t))

(defun backtrack (env undo-label)
  ;; WHEN WE RUN OUT OF ALTERNATIVES, THE STACK SHOULD BE EMPTY
  (iter (while (back-stack env))
	(pop-backtrack (goals tried-facts tried-rules) env
	  (let ((matches (find-unused-matches
			  env (select-goal goals) tried-facts tried-rules)))
            ;; WHEN NO MATCHES, LOOP CONTINUES
	    (when matches
	      (setf (goals env) goals)
              ;; SEVERAL MATCHES CAN BE FOUND IN ONE STEP, THESE SHOULD BE ITERATED
	      (return (make-back-step
		       env (select-match matches) undo-label
                       tried-facts tried-rules)))))))

;; public
;; TODO: this should be undoable

;; RELEVANT STATE: goals, back-stack

;; if there are no more goals, back-step recognizes this as final success
;; and returns nil, which is how back-run knows to stop the cycle

;; possible states:
;; no more goals - answer was found (but may not be the last one)
;; goals present because only partial match was found in last step
;; goals present because there are no more matches
;; => back-run can't test only goals to know whether to continue

;; HOW TO ASK FOR ALTERNATIVE ANSWERS?
;; - run (back-run) again
;;   - we need recognize, whether all matches have been found or only partial
;;     match was found in last step
(defmethod back-step ((env environment) &optional (undo-label "(back-step)"))
  "make one inference step using backward chaining"
  ;; IF NO GOALS, SEE IF WE CAN BACKTRACK
  (if (goals env)
      (let ((matches (find-matches env (select-goal (goals env)))))
	(if matches
            ;; SEVERAL MATCHES CAN BE FOUND IN ONE STEP, THESE SHOULD BE ITERATED
	    (make-back-step env (select-match matches) undo-label)
	    (backtrack env undo-label)))
      (backtrack env undo-label)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-inference-report (env)
  (fresh-format t "~%All goals have been satisfied")
  (dolist (match (back-stack-matches env))
    (print-goal-match match))
  (let ((substitutions (used-substitutions env)))
    (fresh-format t "These variable bindings have been used:~%~A"
		  substitutions)
    substitutions))

(defmethod back-run ((env environment) &optional (undo-label "(back-run)"))
  (with-saved-slots env (goals back-stack) undo-label
    (let (*print-match*)
      (iter (for match-found-p = (back-step env))
            (if match-found-p
                (unless (goals env)
                  (return (print-inference-report env)))
                (return (fresh-format t "No feasible answer found")))))))
