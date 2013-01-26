(in-package :exil-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public, virtual
(defclass pattern () ((negated :initform nil
                               :initarg :negated
                               :accessor negated-p)
                      (match-variable :initform nil
                                      :initarg :match-var
                                      :accessor match-var)))

; public, used by rules
(defgeneric pattern-equal-p (pattern1 pattern2)
  (:method ((pattern1 pattern) (pattern2 pattern)) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public
(defclass simple-pattern (pattern simple-object)
  ((specifier :initform (error "pattern slot must be specified")
              :initarg :pattern
              :reader pattern)))

(defun make-simple-pattern (pattern-spec &optional negated match-var)
  (make-instance 'simple-pattern
                 :pattern (copy-list pattern-spec)
                 :negated negated
                 :match-var match-var))

; public
(defmethod print-object ((pattern simple-pattern) stream)
  (labels ((print-pattern ()
             (format stream "~@[~A <- ~]~:[~;NOT ~]~S" (match-var pattern)
                     (negated-p pattern) (pattern pattern))))
    (if *print-escape*
        (print-unreadable-object (pattern stream :type t)
          (print-pattern))
        (print-pattern)))
  pattern)

; public, used by rules
(defmethod pattern-equal-p ((pattern1 simple-pattern) (pattern2 simple-pattern))
  (and (equalp (pattern pattern1) (pattern pattern2))
       (equalp (negated-p pattern1) (negated-p pattern2))))

; OBSOLETE:
;; checks pattern constant equivalency, ignores variables
;(defmethod pattern-const-equal-p ((pattern1 simple-pattern)
;				  (pattern2 simple-pattern))
;  (and (equalp (negated-p pattern1) (negated-p pattern2))
;       (every #'var-or-equal-p
;	      (pattern pattern1)
;	      (pattern pattern2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public
(defclass template-pattern (pattern template-object) ())

(defmethod slot-default ((type (eql 'pattern)))
  '?)

; private
(defmethod tmpl-pattern-slot-value ((pattern template-pattern) slot-name)
  (tmpl-object-slot-value pattern slot-name))

; public, used by rules
(defmethod pattern-equal-p ((pattern1 template-pattern)
                            (pattern2 template-pattern))
  (and (equalp (negated-p pattern1) (negated-p pattern2))
       (tmpl-object-equal-p pattern1 pattern2)))

; not in use
;(defgeneric pattern-slot (pattern slot-spec)
;  (:documentation "returns pattern's slot specified by slot-spec")
;  (:method ((pattern simple-pattern) (slot-spec integer))
;    (nth slot-spec (pattern pattern)))
;  (:method ((pattern template-pattern) (slot-spec symbol))
;    (tmpl-pattern-slot-value pattern slot-spec)))

; public
(defmethod print-object ((object template-pattern) stream)
  (if *print-escape*
      (print-unreadable-object (object stream :type t :identity t)
        (format stream "~:[~;NOT ~]~A" (negated-p object)
                (cons (tmpl-name object) (slots object))))
      (format stream "~:[~;NOT ~]~A" (negated-p object)
              (cons (tmpl-name object) (slots object))))
  object)

; public, used by rete
(defgeneric atom-equal-p (object1 object2)
  (:documentation "equality predicate for fact atoms")
  (:method (object1 object2) (equalp object1 object2)))

; public, used by rete
(defun constant-test (desired-value real-value)
  (or (variable-p desired-value)
      (atom-equal-p desired-value real-value)))

; public, used by rete
(defun var-or-equal-p (atom1 atom2)
  (or (and (variable-p atom1)
           (variable-p atom2))
      (atom-equal-p atom1 atom2)))
