(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACT MATCHING

;; returns list of goal-matches
(defun find-matching-facts (env goal)
  (iter (for fact :in (facts env))
	(multiple-value-bind (valid-match bindings)
	    (match-against-pattern fact goal)
	  (when valid-match
	    (collect (make-goal-match goal fact bindings))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RULE MATCHING

;; forward declaration
(defgeneric exil-parser:parse-pattern (env pattern-spec &key match-var))

(defun variables-in-goals (goals)
  (remove-duplicates (mapcan #'variables-in-pattern goals)))

(defun rule-rhs-assert-forms (rule)
  (remove-if-not (lambda (form)
		   (equalp (first form) 'assert))
		 (activations rule)))

;; this creates a kind of cyclic dependency between parser and environment
;; - parser needs environment to find templates in, but to match facts, that
;;   will result from firing a rule with subgoals, environment needs parser
;;   to parse assert forms appearing in rules' activations (rhs)
;;
;; we're parsing these forms as patterns, as opposed to forward chaining, which
;; evaluates them and assert parses them as facts, this is because when they're
;; parsed by assert, the variables are already substituted, which obviously
;; doesn't hold for backward chaining
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
      (declare (ignore valid-match))
      (when matching-pattern
	(values t bindings)))))

;; returns list of goal-matches
(defun find-matching-rules (env goal)
  (iter (for (name rule) :in-hashtable (rules env))
	(multiple-value-bind (valid-match bindings)
	    (match-rule-against-goal env rule goal)
	  (when valid-match (collect (make-goal-match goal rule bindings))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL

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
