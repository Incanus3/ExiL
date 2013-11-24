(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BACKWARD CHAINING
;; use case:
;; 1) user specifies the goal (1 or more subgoals in the form of patterns)
;; 2) user starts the backward inference process by calling back-run
;;    or back-step
;;
;; backward-chaining inference works very similarly to prolog's SLD resolution:
;; 1) subgoal (a pattern) is selected (in the order of addition for now)
;;    and removed from the list
;; 2) facts are searched for matches
;;    - if matching fact is found, the variable substitution is applied to the
;;      rest of the subgoals
;; 3) if none is found, rules are searched:
;;    (should rules be stored in order of addition?)
;;    each rule's activations are searched for assert forms with pattern matching
;;    the subgoal
;;    - if such a rule is found, the variable substitution is applied to the
;;      rest of the subgoals and to conditions of the rule, which are then added
;;      to the list of subgoals
;;
;; this is a very limited implementation of backward-chaining inference:
;; - the rules are never actually fired, so the rest of the activations aren't
;;   evaluated, which means
;; 1) any side-effect is lost
;; 2) retract and modify forms aren't considered so the matching rule may
;;    actualy invalidate some of the other goals
;; 3) negative conditions aren't considered, so some other assert form of
;;    the rule's rhs may invalidate some other rule's negative conditions
;;    and when negative conditions of the rule are added to subgoals, they are
;;    then considered as 'positive' subgoals
;;
;; => use this only with prolog-like rules, which
;; 1) have no side-effect
;; 2) have no negative conditions
;; 3) result only in addition of a fact (only assert forms in rhs)
;;    IDEALLY ONLY ONE FORM - THIS IS ACTUALLY A BACKWARD CHAINING
;;    -> IS THIS REALLY AN ISSUE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GOALS

;; public
;; TODO: this should be undoable
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
;; GOAL MATCHES

(defun make-goal-match (goal fact/rule bindings)
  (list goal fact/rule bindings))

(defun goal-match-goal (match)
  (first match))

;; match can hold either fact or rule
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
