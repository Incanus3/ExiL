(in-package :exil-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public, virtual
(defclass pattern () ((negated :initform nil
                               :initarg :negated
                               :accessor negated-p)
                      (match-variable :initform nil
                                      :initarg :match-var
                                      :accessor match-var)))

(defmethod exil-equal-p and ((pattern1 pattern) (pattern2 pattern))
  (equalp (negated-p pattern1) (negated-p pattern2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public
(defclass simple-pattern (pattern simple-object)
  ((specifier :initform (error "pattern slot must be specified")
              :initarg :pattern
              :reader pattern)))

;; exil-equal-p inherited from pattern and simple-object

(defun make-simple-pattern (pattern-spec &optional negated match-var)
  (make-instance 'simple-pattern
                 :pattern (copy-list pattern-spec)
                 :negated negated
                 :match-var match-var))

(defmethod format-object ((pattern simple-pattern) stream)
  (format stream "~@[~A <- ~]~:[~;NOT ~]~S" (match-var pattern)
                     (negated-p pattern) (pattern pattern)))

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

;; exil-equal-p inherited from pattern and template-object

(defmethod slot-default ((type (eql 'pattern)))
  '?)

(defmethod format-object ((object template-pattern) stream)
  (format stream "~:[~;NOT ~]~A" (negated-p object)
                (cons (tmpl-name object) (slots object))))

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
