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

;(defclass pattern () (negated match-variable))
;(defclass simple-pattern (pattern simple-object) (specifier))
;(defun make-simple-pattern (pattern-spec &key negated match-var))
;(defclass template-pattern (pattern template-object) ())
;(defgeneric make-template-pattern (template slot-spec &key negated match-var))
;(defun variable-p (expr))
;(defun constant-test (desired-value real-value))
;(defun var-or-equal-p (atom1 atom2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public, virtual
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

; public
(defclass simple-pattern (pattern simple-object)
  ((specifier :initform (error "pattern slot must be specified")
              :initarg :pattern
              :reader pattern)))

; public
(defun make-simple-pattern (pattern-spec &key negated match-var)
  (make-instance 'simple-pattern
                 :pattern (copy-list pattern-spec)
                 :negated negated
                 :match-var match-var))

;; TODO: change this to correspond to assert format
(defmethod format-object ((pattern simple-pattern) stream)
  (format stream "~@[~A <- ~]~:[~;NOT ~]~A" (match-var pattern)
                     (negated-p pattern) (pattern pattern)))

;;;; inherited from simple-object:
;; print-object, copy-object, object-slot, find-atom, atom-position, description

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public
(defclass template-pattern (pattern template-object) ())

(defmethod format-object ((object template-pattern) stream)
  (format stream "~:[~;NOT ~]~A" (negated-p object)
                (cons (name (template object)) (slots object))))

; public
(defgeneric make-template-pattern (template slot-spec &key negated match-var)
  (:documentation "finds values for pattern's slots, creates new tmpl-patter"))

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
; private
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

(defun every-key-once (alist)
  (every (lambda (pair)
	   (= (count (car pair) alist :key #'car) 1))
	 alist))

; returns cons (var . binding), nil or :mismatch
(defun match-atom% (fact-atom pattern-atom)
  (if (variable-p pattern-atom)
      (cons pattern-atom fact-atom)
      (unless (equalp pattern-atom fact-atom)
	:mismatch)))

(defmethod match-fact-pattern%% ((fact simple-fact) (pattern simple-pattern))
  (if (= (length (fact fact)) (length (pattern pattern)))
      (mapcar #'match-atom% (fact fact) (pattern pattern))
      (list :mismatch)))

(defmethod match-fact-pattern%% ((fact template-fact) (pattern template-pattern))
  (if (exil-equal-p (template fact) (template pattern))
      (iter (for (slot-name . slot-val) :in (slots pattern))
	    (collect (match-atom% (object-slot fact slot-name) slot-val)))
      (list :mismatch)))

;; returns list of variable bindings, which may contain the symbol :mismatch
(defun match-fact-pattern% (fact pattern)
  (remove-duplicates (delete nil (match-fact-pattern%% fact pattern)) :test #'equalp))

(defmethod match-fact-against-pattern ((fact fact) (pattern pattern))
  (let* ((bindings (match-fact-pattern% fact pattern))
	 (match-var (match-var pattern)))
    (if match-var (push (cons match-var (description fact)) bindings))
    (if (and (not (find :mismatch bindings))
	     (every-key-once bindings))
	(values t bindings)
	(values nil nil))))
