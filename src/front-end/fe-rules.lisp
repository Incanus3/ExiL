(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strategies

;; TODO: this shouldn't take function, instead it should seem more like
;; a function definition - it should take name and body, pass the body to parser
;; to create the strategy (which will probably still be just a function)
;; and then store the strategy in the environment under given name
(defun defstrategy% (name function)
  (add-strategy *current-environment* name function
		(format nil "(defstrategy ~A ~A)" name function)))
; public
(defmacro defstrategy (name function)
  "define new strategy"
  `(defstrategy% ',name ,function))

; public
(defmacro setstrategy (name)
  "set strategy to use"
  `(set-strategy *current-environment* ',name
		 (format nil "(setstrategy ~A)" ',name)))

(defun current-strategy ()
  (format nil "~A" (current-strategy-name *current-environment*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rules

(defun defrule% (name body)
  (add-rule *current-environment*
             (parse-rule *current-environment* name body)
	     (format nil "(defrule ~A ~A)" name body)))

; public
(defmacro defrule (name &body body)
  "define rule"
  `(defrule% ',name ',body))

; public
(defmacro undefrule (name)
  "undefine rule"
  `(rem-rule *current-environment* ',name
	     (format nil "(undefrule ~A)" ',name)))

(defun ppdefrule% (name)
  (fresh-princ (find-rule *current-environment* name)))

; public
(defmacro ppdefrule (name)
  "pretty-print rule definition"
  `(ppdefrule% ',name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; agenda

(defun agenda ()
  (print-activations *current-environment*)
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
