(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; application macros

(defun assert% (fact-spec)
  (add-fact (make-fact fact-spec)))

(defmacro assert (fact-spec)
  "Add fact into working memory"
  `(assert% ',fact-spec))

(defun retract% (fact-spec)
  (rem-fact (make-fact fact-spec)))

(defmacro retract (fact-spec)
  "Remove fact from working memory"
  `(retract% ',fact-spec))

(defun modify% (old-fact-spec new-fact-spec)
  (retract% old-fact-spec)
  (assert% new-fact-spec))

(defmacro modify (old-fact-spec new-fact-spec)
  `(modify% ',old-fact-spec ',new-fact-spec))

(defun clear ()
  (reset-environment))

(defmacro deffacts (name &body fact-descriptions)
  "Create group of facts to be asserted after (reset)"
  `(add-fact-group ',name ',fact-descriptions))

;; DODELAT KONTROLU, ZDA SE VSECHNY PROMENNE V RHS VYSKYTUJI V LHS
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

(defmacro undefrule (name)
  (let ((rule (gensym "rule")))
    `(let ((,rule (find-rule ',name)))
       (when ,rule (rem-rule ,rule)))))

(defun assert-group (fact-descriptions)
  (dolist (desc fact-descriptions)
    (assert% desc)))

(defun reset ()
  "Reset the environment"
  (clear)
  (dolist (group (fact-groups))
    (assert-group (cdr group))))

(defun step ()
  (when (agenda)
    (activate-rule (select-activation))
    t))

(defvar *exil-running* nil)

(defun halt ()
  (format t "Halting~%")
  (setf *exil-running* nil))

(defun run ()
  "Run the infenece engine"
  (setf *exil-running* t)
  (loop while (and *exil-running* (step))))
