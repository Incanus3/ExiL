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

(defmacro make-pattern (pattern)
  `(make-instance 'simple-pattern :pattern ',pattern))

;; prints patterns
(defmethod print-object ((pattern simple-pattern) stream)
  (print-unreadable-object (pattern stream :type t)
    (format stream "~s" (pattern pattern))
    pattern))

;; checks pattern equivalency
(defmethod pattern-equal-p ((pattern1 simple-pattern) (pattern2 simple-pattern))
  (equalp (pattern pattern1) (pattern pattern2)))

;; checks pattern constant equivalency, ignores variables
(defmethod pattern-const-equal-p ((pattern1 simple-pattern)
				  (pattern2 simple-pattern))
  (every (lambda (atom1 atom2)
	   (or (and (variable-p atom1)
		    (variable-p atom2))
	       (equalp atom1 atom2)))
	 (pattern pattern1)
	 (pattern pattern2)))

;; although pattern is not fact, i inherit from template-fact class
;; because otherwise i'd have to copy a huge bunch of code, that would be
;; the same as for template-facts
(defclass template-pattern (pattern template-fact) ())

(defmacro tmpl-pattern (pattern-spec)
  `(tmpl-object ,pattern-spec 'template-pattern))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fact matching

(defgeneric atom-equal-p (object1 object2)
  (:documentation "equality predicate for fact atoms")
  (:method (object1 object2) (equalp object1 object2)))

;; terminology note:
;; be sure about distiction of following expressions
;; a) template-facts - instances of class template-fact
;;      - they're just facts with named slots
;;      - e.g. (car :color red :mph 160)
;; b) templates - instances of class template
;;      - prescriptions for template-facts - describe the slot names,
;;        default values, etc.
;;      - you can't create template-fact without having a template for it
;; c) patterns - could be either in simple-fact or in template-fact form
;;      - they're not facts, they can include wildcards, variables, etc.
;;      - e.g. (on red-box ?some-other-box)
;;      - or (car :color ?some-color)

(defgeneric variable-bindings (pattern fact)
  (:documentation "if the fact passes the pattern,
                     returns the variable bindings in assoc list"))

(defun variable-p (expr)
  (and (symbolp expr)
       (char-equal (char (symbol-name expr) 0) #\?)))

;; checks consistency of constant atoms in pattern, ignores variables
;; returns fact if passed, nil otherwise
(defmethod constant-check ((pattern simple-pattern)
			   (fact simple-fact))
  (when (every (lambda (pt-atom atom)
		 (or (variable-p pt-atom)
		     (atom-equal-p pt-atom atom)))
	       (pattern pattern)
	       (fact fact))
    fact))

;; could use hash-table instead of assoc-list, if the facts has many atoms
;; in need of longer facts could be 2 methods (with assoc-list and hash table)
;; and one that would call them depending on length
(defmethod variable-bindings ((pattern simple-pattern)
			      (fact simple-fact))
  (loop
     with bindings = ()
     for pt-atom in (pattern pattern)
     for atom in (fact fact)
     do (if (variable-p pt-atom)
	    (let ((binding (cdr (assoc pt-atom bindings))))
	      (if binding
		  (unless (atom-equal-p binding atom)
		    (return nil))
		  (push (cons pt-atom atom) bindings)))
	    (unless (atom-equal-p pt-atom atom)
	      (return nil)))
     finally (return (nreverse bindings))))

