(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strategies

;; TODO: this shouldn't take function, instead it should seem more like
;; a function definition - it should take name and body, pass the body to parser
;; to create the strategy (which will probably still be just a function)
;; and then store the strategy in the environment under given name
(defun defstrategyf (name function)
  (add-strategy *current-environment* name function
		(format nil "(defstrategy ~A ~A)" name function))
  nil)

;; public
(defmacro defstrategy (name function)
  "define new strategy"
  `(defstrategyf ',name ,function))

(defun undefstrategyf (name)
  (rem-strategy *current-environment* name
		(format nil "(undefstrategy ~A)" name))
  nil)

;; public
(defmacro undefstrategy (name)
  "define new strategy"
  `(undefstrategyf ',name))

(defun setstrategyf (name)
  "set strategy to use"
  (set-strategy *current-environment* name
                (format nil "(setstrategy ~A)" name))
  nil)

; public
(defmacro setstrategy (name)
  "set strategy to use"
  `(setstrategyf ',name))

(defun current-strategy ()
  (current-strategy-name *current-environment*))

(defun strategies ()
  (strategy-names *current-environment*))

(defun find-strategy (name)
  (eenv:find-strategy *current-environment* name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rules

(defun defrulef (name body)
  (add-rule *current-environment*
             (parse-rule *current-environment* name body)
	     (format nil "(defrule ~A ~A)" name body)))

;; public
(defmacro defrule (name &body body)
  "define rule"
  `(defrulef ',name ',body))

(defun undefrulef (name)
  (rem-rule *current-environment* name
            (format nil "(undefrule ~A)" name)))

;; public
(defmacro undefrule (name)
  "undefine rule"
  `(undefrulef ',name))

(defun rules ()
  (rule-names *current-environment*))

(defun find-rulef (name)
  (external (exil-env:find-rule *current-environment* name)))

(defmacro find-rule (name)
  `(find-rulef ',name))

(defun ppdefrulef (name)
  (fresh-princ (exil-env:find-rule *current-environment* name)))

;; public
(defmacro ppdefrule (name)
  "pretty-print rule definition"
  `(ppdefrulef ',name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; agenda

(defun agenda ()
  (print-agenda *current-environment*)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backward chaining

(defun defgoalf (goal-spec)
  (add-goal *current-environment* (parse-pattern *current-environment* goal-spec))
  nil)

(defmacro defgoal (goal-spec)
  `(defgoalf ',goal-spec))

(defun goals ()
  (mapcar #'external (exil-env:goals *current-environment*)))
