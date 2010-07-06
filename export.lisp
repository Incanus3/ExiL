(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; application macros

(defun %assert (fact-spec)
  (let ((fact (if (tmpl-fact-specification-p fact-spec)
		  (tmpl-fact fact-spec)
		  (make-instance 'simple-fact :fact fact-spec))))
    (add-fact fact)))

(defmacro assert (fact-spec)
  "Add fact into working memory"
  `(%assert ',fact-spec))

(defmacro retract (fact &optional)
  "Remove fact from working memory"
  (declare (ignorable fact)))

(defmacro deffacts (name &body fact-descriptions)
  "Create group of facts to be asserted after (reset)"
  `(add-fact-group ',name ',fact-descriptions))

(defmacro defrule (name &body rule)
  "Define rule"
  (let ((=>-position (position '=> rule))
	(rule-symbol (gensym)))
    (cl:assert =>-position ()
	    "rule definition must include =>")
    `(let ((,rule-symbol
	    (make-instance
	     'rule
	     :name ',name
	     :conditions (mapcar #'make-pattern ',(subseq rule 0 =>-position))
	     :activations ',(subseq rule (1+ =>-position)))))
       (add-rule ,rule-symbol))))

(defun assert-group (fact-descriptions)
  (dolist (desc fact-descriptions)
    (%assert desc)))

(defun reset ()
  "Reset the environment"
  (reset-environment)
  (dolist (group (fact-groups))
    (assert-group (cdr group))))

(defun run ()
  "Run the infenece engine"

  )
