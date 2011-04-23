(in-package :exil-core)

; public, used by rete
(defgeneric atom-equal-p (object1 object2)
  (:documentation "equality predicate for fact atoms")
  (:method (object1 object2) (equalp object1 object2)))

; public, used by rete
(defun constant-test (desired-value real-value)
  (or (variable-p desired-value)
      (atom-equal-p desired-value real-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pattern classes

;; virtual class pattern
; public
(defclass pattern () ((negated :initform nil
			       :initarg :negated
			       :accessor negated-p)))

;; pattern equality predicate
; public, used by rules
(defgeneric pattern-equal-p (pattern1 pattern2)
  (:method ((pattern1 pattern) (pattern2 pattern)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class simple-pattern
; public
(defclass simple-pattern (pattern)
  ((pattern :initform (error "pattern slot must be specified")
	    :initarg :pattern
	    :reader pattern)))

;; prints patterns
; public
(defmethod print-object ((pattern simple-pattern) stream)
  (if *print-escape*
      (print-unreadable-object (pattern stream :type t)
	(format stream "~:[~;NOT ~]~S" (negated-p pattern)
		(pattern pattern)))
      (format stream "~:[~;NOT ~]~S" (negated-p pattern)
	      (pattern pattern)))
  pattern)

;; checks pattern equivalency
; public, used by rules
(defmethod pattern-equal-p ((pattern1 simple-pattern) (pattern2 simple-pattern))
  (and (equalp (pattern pattern1) (pattern pattern2))
       (equalp (negated-p pattern1) (negated-p pattern2))))

; public, used by rete
(defun var-or-equal-p (atom1 atom2)
  (or (and (variable-p atom1)
	   (variable-p atom2))
      (atom-equal-p atom1 atom2)))

; OBSOLETE:
;; checks pattern constant equivalency, ignores variables
;(defmethod pattern-const-equal-p ((pattern1 simple-pattern)
;				  (pattern2 simple-pattern))
;  (and (equalp (negated-p pattern1) (negated-p pattern2))
;       (every #'var-or-equal-p
;	      (pattern pattern1)
;	      (pattern pattern2))))

; public, used by rete
(defmethod find-atom (atom (pattern simple-pattern))
  (find atom (pattern pattern)))

; public, used by rete
(defmethod atom-position (atom (pattern simple-pattern))
  (position atom (pattern pattern)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; although pattern is not fact, i inherit from template-fact class
;; because otherwise i'd have to copy a huge bunch of code, that would be
;; the same as for template-facts
; public
(defclass template-pattern (pattern template-object)
  ((slot-default :initform '? :allocation :class)))

; private
(defmethod tmpl-pattern-slot-value ((pattern template-pattern) slot-name)
  (tmpl-object-slot-value pattern slot-name))

; public, used by rules
(defmethod pattern-equal-p ((pattern1 template-pattern) (pattern2 template-pattern))
  (and (equalp (negated-p pattern1) (negated-p pattern2))
       (tmpl-object-equal-p pattern1 pattern2)))

; not in use
;(defgeneric pattern-slot (pattern slot-spec)
;  (:documentation "returns pattern's slot specified by slot-spec")
;  (:method ((pattern simple-pattern) (slot-spec integer))
;    (nth slot-spec (pattern pattern)))
;  (:method ((pattern template-pattern) (slot-spec symbol))
;    (tmpl-pattern-slot-value pattern slot-spec)))

; private
(defun make-tmpl-pattern (pattern-spec &optional (negated nil))
  (let ((pattern (make-tmpl-object pattern-spec 'template-pattern)))
    (setf (negated-p pattern) negated)
    pattern))

; private
(defun tmpl-pattern-specification-p (specification)
  (tmpl-object-specification-p specification))

; public, used by front-end
(defun make-pattern (specification)
  (let* ((negated (equalp (first specification) '-))
	 (spec (if negated (rest specification) specification)))
    (if (tmpl-pattern-specification-p spec)
	(make-tmpl-pattern spec negated)
	(make-instance 'simple-pattern :pattern spec :negated negated))))

; public
(defmethod print-object ((object template-pattern) stream)
  (if *print-escape*
      (print-unreadable-object (object stream :type t :identity t)
	(format stream "~:[~;NOT ~]~A" (negated-p object)
		(cons (tmpl-name object) (slots object))))
      (format stream "~:[~;NOT ~]~A" (negated-p object)
	      (cons (tmpl-name object) (slots object))))
  object)
