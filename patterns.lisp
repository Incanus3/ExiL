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

(defun make-simple-pattern (pattern-spec &optional negated match-var)
  (make-instance 'simple-pattern
                 :pattern (copy-list pattern-spec)
                 :negated negated
                 :match-var match-var))

(defmethod format-object ((pattern simple-pattern) stream)
  (format stream "~@[~A <- ~]~:[~;NOT ~]~S" (match-var pattern)
                     (negated-p pattern) (pattern pattern)))

;;;; inherited from simple-object:
;; print-object, copy-object, object-slot, find-atom, atom-position, description

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public
(defclass template-pattern (pattern template-object) ())

(defmethod format-object ((object template-pattern) stream)
  (format stream "~:[~;NOT ~]~A" (negated-p object)
                (cons (tmpl-name object) (slots object))))

;;;; inherited from template-object:
;; print-object, copy-object, object-slot, find-atom, atom-position,
;; description, has-slot-p

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod slot-default ((type (eql 'pattern)))
  '?)

; public, used by rete
(defun constant-test (desired-value real-value)
  (or (variable-p desired-value)
      (equalp desired-value real-value)))

; public, used by rete
(defun var-or-equal-p (atom1 atom2)
  (or (and (variable-p atom1)
           (variable-p atom2))
      (equalp atom1 atom2)))
