(in-package :exil)

; private
(defparameter *clips-mode* nil)

; public
(defun set-clips-mode (val)
  (setf *clips-mode* val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; application macros

(defmacro deftemplate (name fields)
  (flet ((field->slot-designator (field)
	   (destructuring-bind (name &key (default nil)) field
	     `(,name . (:default ,default)))))

    (let ((template (gensym "template")))

      `(let ((,template
	      (make-template ',name
			     ',(loop for field in (to-list-of-lists fields)
				  collect (field->slot-designator field)))))
	 (add-template ,template)))))

(defun facts (&optional (start-index 0) (end-index (length (exil-env:facts)))
			(at-most end-index))
  (let ((facts (exil-env:facts)))
    (loop for i from start-index to (min end-index (+ start-index at-most -1))
       collect (nth i facts))))

(defun assert% (fact-spec)
  (add-fact (make-fact fact-spec)))

(defmacro assert (&rest fact-specs)
  "Add fact into working memory"
  (let ((fact-spec (gensym "fact-spec")))
    `(dolist (,fact-spec ',fact-specs)
       (assert% ,fact-spec))))

(defun retract% (fact-specs)
  (let (facts-to-remove)
    (dolist (fact-spec fact-specs)
      (typecase fact-spec
	(list (pushnew (make-fact fact-spec) facts-to-remove))
	(integer (pushnew (nth (1- fact-spec) (facts)) facts-to-remove))
	(t (error "Type ~A not supported by retract" (type-of fact-spec)))))
    (dolist (fact facts-to-remove)
      (rem-fact fact))))

; retract supports either full fact specification e.g. (retract (is-animal duck))
; or number indices (starting with 1) for clips compatitibity.
; It can't support * to retract all facts as clips does, cause this symbol has
; a special meaning in lisp. retract-all does this instead.
(defmacro retract (&rest fact-specs)
  "Remove fact from working memory"
  `(retract% ',fact-specs))

(defun retract-all ()
  (reset-facts))

(defun modify% (old-fact-spec new-fact-spec)
  (retract% (list old-fact-spec))
  (assert% new-fact-spec))

(defmacro modify (old-fact-spec new-fact-spec)
  "Replace old-fact by new-fact"
  `(modify% ',old-fact-spec ',new-fact-spec))

(defun clear ()
  "Delete all facts"
  (reset-environment))

(defmacro deffacts (name &body fact-descriptions)
  "Create group of facts to be asserted after (reset)"
  (if (stringp (first fact-descriptions)) (pop fact-descriptions))
  `(add-fact-group ',name ',fact-descriptions))

(defmacro undeffacts (name)
  "Delete fact group"
  `(rem-fact-group ',name))

(defun assert-group% (fact-descriptions)
  (dolist (desc fact-descriptions)
    (assert% desc)))

(defun reset ()
  "Clear all facts and add all fact groups"
  (clear)
  (dolist (group (fact-groups))
    (assert-group% (cdr group))))

(defun my-position (atom list)
  (position atom list))

;; DODELAT KONTROLU, ZDA SE VSECHNY PROMENNE V RHS VYSKYTUJI V LHS
(defmacro defrule (name &body rule)
  "Define rule"
  (when (stringp (first rule))
    (setf rule (rest rule))) ;; ignore the clips rule header
  (let ((=>-position (position '=> rule :test #'weak-symbol-equal-p))
	(rule-symbol (gensym)))
    (cl:assert =>-position ()
	    "rule definition must include =>")
    `(let ((,rule-symbol
	    (make-rule ',name
		       (mapcar #'make-pattern ',(subseq rule 0 =>-position))
		       ',(subseq rule (1+ =>-position)))))
       (add-rule ,rule-symbol))))

(defun ppdefrule% (name)
  (let ((rule (find-rule name)))
    (format t "(defrule ~A~{~%  ~A~}~%  =>~{~%  ~S~})"
	    name (conditions rule) (activations rule))))

(defmacro ppdefrule (name)
  `(ppdefrule% ',name))

(defmacro undefrule (name)
  "Undefine rule"
  (let ((rule (gensym "rule")))
    `(let ((,rule (find-rule ',name)))
       (when ,rule (rem-rule ,rule)))))

(defmacro defstrategy (name function)
  "Define strategy"
  `(add-strategy ',name ,function))

(defmacro setstrategy (name)
  "Set strategy to use"
  `(set-strategy ',name))

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
  `(progn (if (weak-symbol-equal-p ',watcher 'all)
	      (watch-all)
	      (set-watcher ',watcher))
	  nil))

(defmacro unwatch (watcher)
  "Unwatch selected item"
  `(progn (if (weak-symbol-equal-p ',watcher 'all)
	      (unwatch-all)
	      (unset-watcher ',watcher))
	  nil))

