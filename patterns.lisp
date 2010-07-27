(in-package :exil)

(defgeneric atom-equal-p (object1 object2)
  (:documentation "equality predicate for fact atoms")
  (:method (object1 object2) (equalp object1 object2)))

(defun constant-test (desired-value real-value)
  (or (variable-p desired-value)
      (atom-equal-p desired-value real-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pattern classes

;; virtual class pattern
(defclass pattern () ())

;; pattern equality predicate
(defgeneric pattern-equal-p (pattern1 pattern2)
  (:method ((pattern1 pattern) (pattern2 pattern)) nil))

;; class simple-pattern
(defclass simple-pattern (pattern)
  ((fact :initform (error "fact slot must be specified")
	 :initarg :pattern
	 :reader pattern)))

;(defmacro make-pattern (pattern)
;  `(make-instance 'simple-pattern :pattern ',pattern))

;; prints patterns
(defmethod print-object ((pattern simple-pattern) stream)
  (print-unreadable-object (pattern stream :type t)
    (format stream "~s" (pattern pattern))
    pattern))

;; checks pattern equivalency
(defmethod pattern-equal-p ((pattern1 simple-pattern) (pattern2 simple-pattern))
  (equalp (pattern pattern1) (pattern pattern2)))

(defun var-or-equal-p (atom1 atom2)
  (or (and (variable-p atom1)
	   (variable-p atom2))
      (atom-equal-p atom1 atom2)))

;; checks pattern constant equivalency, ignores variables
(defmethod pattern-const-equal-p ((pattern1 simple-pattern)
				  (pattern2 simple-pattern))
  (every #'var-or-equal-p
	 (pattern pattern1)
	 (pattern pattern2)))

(defmethod find-atom (atom (pattern simple-pattern))
  (find atom (pattern pattern)))

(defmethod atom-postition (atom (pattern simple-pattern))
  (position atom (pattern pattern)))

;; although pattern is not fact, i inherit from template-fact class
;; because otherwise i'd have to copy a huge bunch of code, that would be
;; the same as for template-facts
(defclass template-pattern (pattern template-object)
  ((slot-default :initform '? :allocation :class)))



(defmethod tmpl-pattern-slot-value ((pattern template-pattern) slot-name)
  (tmpl-object-slot-value pattern slot-name))

(defmethod pattern-equal-p ((pattern1 template-pattern) (pattern2 template-pattern))
  (tmpl-object-equal-p pattern1 pattern2))

(defgeneric pattern-field (pattern field)
  (:documentation "returns pattern's field")
  (:method ((pattern simple-pattern) (field integer))
    (nth field (pattern pattern)))
  (:method ((pattern template-pattern) (field symbol))
    (tmpl-pattern-slot-value pattern field)))
