(in-package :exil)

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

;; although pattern is not fact, i inherit from template-fact class
;; because otherwise i'd have to copy a huge bunch of code, that would be
;; the same as for template-facts
(defclass template-pattern (pattern template-object) ())

(defun tmpl-pattern (pattern-spec)
  (tmpl-object pattern-spec 'template-pattern))

(defun tmpl-pattern-specification-p (specification)
  (tmpl-object-specification-p specification))

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

(defun make-pattern (specification)
  (if (tmpl-fact-specification-p specification)
      (tmpl-pattern specification)
      (make-instance 'simple-pattern :pattern specification)))
