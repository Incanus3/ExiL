(in-package :exil-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are the expert system's inference rules, they consist of a set of
;; conditions (which are patterns) and a set of activations, which may be any
;; lisp expressions and can contain variables, that appear in the conditions.
;; These expressions are evaluated when all the rule's conditions are met
;; (facts matching the patterns with compatible variable bindings are found)
;; and the rule is selected from agenda (defined in environment) for activation
;; - we say the rule is "fired". Rule's activations typically consist of calls
;; to assert, retract or modify, thus modifying the system's knowledge base,
;; but the user is not limited to these options, printing output is a typical
;; example of other usage.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public
(defclass rule ()
  ((name :initarg :name :reader name)
   (conditions :initarg :conditions :reader conditions)
   (activations :initarg :activations :reader activations)))

#|
(defmethod rule-equal-p ((rule1 rule) (rule2 rule))
  (with-slots ((name1 name)
               (conds1 conditions)
               (acts1 activations)) rule1
    (with-slots ((name2 name)
                 (conds2 conditions)
                 (acts2 activations)) rule2
      (and (equalp name1 name2)
           (every #'weak-equal-p conds1 conds2)
           (every #'weak-equal-p acts1 acts2)))))
|#

; public
(defmethod rule-equal-p ((rule1 rule) (rule2 rule))
  (equalp (name rule1) (name rule2)))

; public
(defmethod print-object ((rule rule) stream)
  (if *print-escape*
      (print-unreadable-object (rule stream :type t)
        (format stream "~A" (name rule)))
      (format stream "Rule ~A:~%~{~A~%~}=>~%~{~A~}" (name rule) (conditions rule)
              (activations rule)))
  rule)

;public
(defmethod make-rule (name conditions activations)
  (make-instance 'rule :name name :conditions conditions :activations activations))
