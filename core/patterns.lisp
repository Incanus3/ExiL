(in-package :exil-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By patterns the infenrence rules' conditions are represented. They're quite
;; similar to facts, but they may contain variables - special symbols whose
;; names start by ?, and which represent wildcards in the conditions. When
;; the rete mechanism tries to find a match for a given rule, it checks mutual
;; compatibility of the variable bindings between the conditions as well as
;; within the condition itself - e.g. when there's the same variable used in
;; several conditions or in several places within one condition, the matching
;; fact(s) have to have the same values in those positions. One exception is
;; the anonymous variable '?, which isn't checked for binding compatibility -
;; it's as if each of its occurrence was actually a different variable (it very
;; similar to prolog's anonymous variable _). The anonymous variable '? may
;; not appear in the rule's activations (see rules).
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

(defun make-simple-pattern (pattern-spec &key negated match-var)
  (make-instance 'simple-pattern
                 :pattern (copy-list pattern-spec)
                 :negated negated
                 :match-var match-var))

;; TODO: change this to correspond to assert format
(defmethod format-object ((pattern simple-pattern) stream)
  (format stream "~@[~A <- ~]~:[~;NOT ~]~S" (match-var pattern)
                     (negated-p pattern) (pattern pattern)))

;;;; inherited from simple-object:
;; print-object, copy-object, object-slot, find-atom, atom-position, description

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public
(defclass template-pattern (pattern template-object) ())

(defmethod format-object ((object template-pattern) stream)
  (format stream "~:[~;NOT ~]~S" (negated-p object)
                (cons (name (template object)) (slots object))))

(defmethod make-template-pattern ((tmpl template) (slot-spec list)
                                  &key negated match-var)
  (let ((pattern (make-tmpl-object tmpl slot-spec 'template-pattern)))
    (setf (negated-p pattern) negated)
    (setf (match-var pattern) match-var)
    pattern))

;;;; inherited from template-object:
;; print-object, copy-object, object-slot, find-atom, atom-position,
;; description, has-slot-p

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pattern's slot, for which user supplies no value and it's default value isn't
;; specified in the template definition, defaults to the anonymous variable '?
; public, used by object-makers (environment)
(defmethod slot-default ((type (eql 'template-pattern)))
  '?)

; public
(defun variable-p (expr)
  "is expr an exil variable?"
  (and (symbolp expr)
       (char-equal (char (symbol-name expr) 0) #\?)))

; public, used by rete
(defun constant-test (desired-value real-value)
  (or (variable-p desired-value)
      (equalp desired-value real-value)))

; public, used by rete
(defun var-or-equal-p (atom1 atom2)
  (or (and (variable-p atom1)
           (variable-p atom2))
      (equalp atom1 atom2)))
