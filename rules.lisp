(in-package :exil-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rules

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
	   (every #'exil-equal-p conds1 conds2)
	   (   )))))
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
