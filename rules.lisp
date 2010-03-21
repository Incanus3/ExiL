(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rules

(defclass rule ()
  ((name :initarg :name :reader name)
   (lhs :initarg :lhs :reader lhs)
   (rhs :initarg :rhs :reader rhs)))

(defmacro defrule (name &body rule)
  "Define rule"
  (let ((=>-position (position '=> rule))
	(rule-symbol (gensym)))
    `(let ((,rule-symbol
	    (make-instance
	     'rule
	     :name ',name
	     :lhs ',(subseq rule 0 =>-position)
	     :rhs ',(subseq rule (1+ =>-position)))))
       (push ,rule-symbol *rules*)
       ,rule-symbol)))

(defun reset ()
  "Reset the environment"

  )

(defun run ()
  "Run the infenece engine"

  )
