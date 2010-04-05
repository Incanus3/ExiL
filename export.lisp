(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; application macros

(defmacro assert (fact-spec &optional (environment *current-environment*))
  "Add fact into working memory"
  (let ((fact (gensym "fact")))
    `(let ((,fact (if (tmpl-fact-p ',fact-spec)
		      (tmpl-fact ,fact-spec)
		      (make-instance 'simple-fact :fact ',fact-spec))))
       (my-pushnew ,fact (facts ,environment) :test #'fact-equal-p)))))

(defmacro retract (fact)
  "Remove fact from working memory"
  (declare (ignorable fact))
  )

(defmacro deffacts (facts-list)
  "Create group of facts to be asserted after (reset)"
  (declare (ignorable facts-list))
  )

(defmacro defrule (name &body rule)
  "Define rule"
  (let ((=>-position (position '=> rule))
	(rule-symbol (gensym)))
    `(let ((,rule-symbol
	    (make-instance
	     'rule
	     :name ',name
	     :conditions ',(subseq rule 0 =>-position)
	     :activations ',(subseq rule (1+ =>-position)))))
       (add-rule ,rule-symbol))))

(defun reset ()
  "Reset the environment"

  )

(defun run ()
  "Run the infenece engine"

  )
