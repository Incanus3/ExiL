(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rules

(defclass rule ()
  ((name :initarg :name :reader name)
   (conditions :initarg :conditions :reader conditions)
   (activations :initarg :activations :reader activations)))

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
       (push ,rule-symbol *rules*)
       ,rule-symbol)))

(defun reset ()
  "Reset the environment"

  )

(defun run ()
  "Run the infenece engine"

  )
