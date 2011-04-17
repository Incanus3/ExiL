(in-package :exil)

(defparameter *clips-mode* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; application macros

(defmacro deftemplate (name fields)
  (flet ((field->slot-designator (field)
	   (destructuring-bind (name &key (default nil)) field
	     `(,name . (:default ,default)))))

    (let ((template (gensym "template")))

      `(let ((,template
	      (make-instance
	       'template
	       :name ',name
	       :slots ',(loop for field in (to-list-of-lists fields)
			   collect (field->slot-designator field)))))
	 (add-template ,template)))))

(defun assert% (fact-spec)
  (add-fact (make-fact fact-spec)))

(defmacro assert (&rest fact-specs)
  "Add fact into working memory"
  (let ((fact-spec (gensym "fact-spec")))
    `(dolist (,fact-spec ',fact-specs)
       (assert% ,fact-spec))))

(defun retract% (fact-spec)
  (rem-fact (make-fact fact-spec)))

(defmacro retract (fact-spec)
  "Remove fact from working memory"
  `(retract% ',fact-spec))

(defun modify% (old-fact-spec new-fact-spec)
  (retract% old-fact-spec)
  (assert% new-fact-spec))

(defmacro modify (old-fact-spec new-fact-spec)
  "Replace old-fact by new-fact"
  `(modify% ',old-fact-spec ',new-fact-spec))

(defun clear ()
  "Delete all facts"
  (reset-environment))

(defmacro deffacts (name &body fact-descriptions)
  "Create group of facts to be asserted after (reset)"
  `(add-fact-group ',name ',fact-descriptions))

(defun assert-group (fact-descriptions)
  (dolist (desc fact-descriptions)
    (assert% desc)))

(defun reset ()
  "Clear all facts and add all fact groups"
  (clear)
  (dolist (group (fact-groups))
    (assert-group (cdr group))))

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
  "Undefine rule"
  (let ((rule (gensym "rule")))
    `(let ((,rule (find-rule ',name)))
       (when ,rule (rem-rule ,rule)))))

(defmacro defstrategy (name function)
  "Define strategy"
  `(defstrategy% ',name ,function))

(defmacro set-strategy (name)
  "Set strategy to use"
  `(set-strategy% ',name))

(defun step ()
  "Run inference engine for one turn"
  (when (agenda)
    (activate-rule (select-activation))
    t))

(defvar *exil-running* nil)

(defun halt ()
  "Stop the inference engine"
  (format t "Halting~%")
  (setf *exil-running* nil))

(defun run ()
  "Run the infenece engine"
  (setf *exil-running* t)
  (loop while (and *exil-running* (step))))

(defmacro watch (watcher)
  "Watch selected item (facts, rules, activations)"
  `(watch% ',watcher))

(defmacro unwatch (watcher)
  "Unwatch selected item"
  `(unwatch% ',watcher))

