(in-package :exil-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; By patterns the infenrence rules' conditions are represented. They're quite
;; similar to facts, but they may contain variables - special symbols whose
;; names start by ?, and which represent wildcards in the conditions. When
;; the rete mechanism tries to find a match for a given rule, it checks mutual
;; consistency of the variable bindings between the conditions as well as
;; within the condition itself - e.g. when there's the same variable used in
;; several conditions or in several places within one condition, the matching
;; fact(s) have to have the same values in those positions. One exception is
;; the anonymous variable '?, which isn't checked for binding consistency -
;; it's as if each of its occurrence was actually a different variable
;; (similar to prolog's anonymous variable _). The anonymous variable '? may
;; not appear in the rule's activations (see rules).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; public interface:

;(defun variable-p (expr))
;(defun constant-test (desired-value real-value))
;(defun var-or-equal-p (atom1 atom2))

;(defclass pattern () (negated match-variable))

;(defclass simple-pattern (pattern simple-object) (specifier))
;(defun make-simple-pattern (pattern-spec &key negated match-var))
;;;; inherited from simple-object:
;; print-object, copy-object, object-slot, find-atom, atom-position, description

;(defclass template-pattern (pattern template-object) ())
(defgeneric make-template-pattern (template slot-spec &key negated match-var)
  (:documentation "finds values for pattern's slots, creates new tmpl-patter"))
;;;; inherited from template-object:
;; print-object, copy-object, object-slot, find-atom, atom-position,
;; description, has-slot-p

(defgeneric variables-in-pattern (pattern))
(defgeneric match-against-pattern (object pattern))
(defgeneric substitute-variables (pattern bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun variable-p (expr)
  "is expr an exil variable?"
  (and (symbolp expr)
       (char-equal (char (symbol-name expr) 0) #\?)))

(defun variables-in-list (list)
  (remove-duplicates (remove-if-not #'variable-p list)))

(defun constant-test (desired-value real-value)
  (or (variable-p desired-value)
      (equalp desired-value real-value)))

(defun var-or-equal-p (atom1 atom2)
  (or (and (variable-p atom1)
           (variable-p atom2))
      (equalp atom1 atom2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass pattern () ((negated :initform nil
                               :initarg :negated
                               :accessor negated-p)
                      (match-variable :initform nil
                                      :initarg :match-var
                                      :accessor match-var)))

(defmethod exil-equal-p ((pattern1 pattern) (pattern2 pattern))
  (and (call-next-method)
       (equalp (negated-p pattern1) (negated-p pattern2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (format stream "~@[~A <- ~]~:[~;NOT ~]~A" (match-var pattern)
                     (negated-p pattern) (pattern pattern)))

(defmethod variables-in-pattern ((pattern simple-pattern))
  (variables-in-list (pattern pattern)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass template-pattern (pattern template-object) ())

(defmethod format-object ((object template-pattern) stream)
  (format stream "~:[~;NOT ~]~A" (negated-p object)
                (cons (name (template object)) (slots object))))

(defmethod make-template-pattern ((tmpl template) (slot-spec list)
                                  &key negated match-var)
  (let ((pattern (make-tmpl-object tmpl slot-spec 'template-pattern)))
    (setf (negated-p pattern) negated)
    (setf (match-var pattern) match-var)
    pattern))

;; pattern's slot, for which user supplies no value and it's default value isn't
;; specified in the template definition, defaults to the anonymous variable '?
(defmethod slot-default ((type (eql 'template-pattern)))
  '?)

(defmethod variables-in-pattern ((pattern template-pattern))
  (variables-in-list (slot-values pattern)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun subst-var (var bindings)
  (let ((binding (assoc-value var bindings)))
    (if binding binding var)))

(defmethod substitute-variables ((pattern simple-pattern) (bindings list))
  (make-simple-pattern
   (mapcar (lambda (atom) (subst-var atom bindings)) (pattern pattern))
   :negated (negated-p pattern)))

(defmethod substitute-variables ((pattern template-pattern) (bindings list))
  (make-template-pattern
   (template pattern)
   (iter (for (slot-name . slot-val) :in (slots pattern))
	 (nconcing (list slot-name (subst-var slot-val bindings))))
   :negated (negated-p pattern)
   :match-var (match-var pattern)))
