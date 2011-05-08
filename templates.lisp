(in-package :exil-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general-purpose functions

; public
(defun variable-p (expr)
  "is expr an exil variable?"
  (and (symbolp expr)
       (char-equal (char (symbol-name expr) 0) #\?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; template class

;; stores template for template facts and patterns
;; slot "slots" holds alist of slot specifiers (plists):
;; (<name> . (:default <default> [:type <type> \ planned \])
;; it is a bit redundant, since there's only one supported option
;; so far, but it's easily extensible
; public
(defclass template ()
  ((name :reader name :initarg :name
	 :initform (error "name slot has to be specified"))
   (slots :reader slots :initarg :slots
	  :initform (error "slots slot has to be specified"))))

; not in use
;(defmethod tmpl-slot-spec ((template template) slot-name)
;  (assoc-value slot-name (slots template)))

; not in use
;(defmethod tmpl-equal-p ((tmpl1 template) (tmpl2 template))
;  (and (equalp (name tmpl1) (name tmpl2))
;       (equalp (slots tmpl1) (slots tmpl2))))

; public
(defmethod print-object ((tmpl template) stream)
  (print-unreadable-object (tmpl stream :type t)
    (format stream "~A ~S" (name tmpl) (slots tmpl)))
  tmpl)

; public
(defun make-template (name slots)
  (make-instance 'template :name name :slots slots))

;; creates instance of template class with given name and slot specification
;; and pushes it into *templates*.
;; it is to consider whether lambda list (name slots)
;; or (name &body slots) is better
;; for the former possibility, the call is more similar to defclass
;; for the latter, the call is more like defstruct call

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; template-object class

;; virtual template-object class, template-fact and template-pattern will
;; inherit from this one
;; slot slots holds alist of slot names and values
; private for package
(defclass template-object ()
  ((template-name :reader tmpl-name :initarg :tmpl-name
		  ;; error would be better, but than class-slot-value
		  ;; wouldn't work, cause it doesn't provide the init value
;		  :initform (error "template-name has to be specified")
		  :initform nil)
   (slot-default :initform nil :allocation :class)
   (slots :reader slots :initarg :slots
	  :initform ())))

; private for package
(defmethod tmpl-object-slot-value ((object template-object) slot-name)
  "get the template-object slot value according to the slot name"
  (assoc-value slot-name (slots object) :test #'weak-symbol-equal-p))

; public
(defmethod has-slot-p ((object template-object) slot-name)
  (find slot-name (slots object) :key #'car :test #'weak-symbol-equal-p))

(defmethod (setf tmpl-object-slot-value) (val (object template-object) slot-name)
  (unless (has-slot-p object slot-name)
    (error "setf tmpl-object-slot-value: ~A doesn't have slot called ~A"
	   object slot-name))
  (setf (assoc-value slot-name (slots object) :test #'weak-symbol-equal-p) val))

; private for package
(defmethod tmpl-object-equal-p ((object1 template-object) (object2 template-object))
  "template-object equality predicate"
  (and (exil-weak-equal-p (tmpl-name object1) (tmpl-name object2))
       (exil-weak-equal-p (slots object1) (slots object2))))

; public
(defmethod print-object ((object template-object) stream)
  "template-object printing method"
  (if *print-escape*
      (print-unreadable-object (object stream :type t :identity t)
	(format stream "~A" (cons (tmpl-name object) (slots object))))
      (format stream "~A" (cons (tmpl-name object) (slots object))))
  object)

; public, called by rete
(defmethod find-atom ((object template-object) atom)
  "find the given atom in template-object slots"
  (find atom (mapcar #'cdr (slots object))))

; public, used by rete
(defmethod atom-position ((object template-object) atom)
  "get the atom position in template-object slots"
  (assoc-key atom (slots object)))

; TODO: supply tmpl-object creation for clips-based slot-spec notation
(defun make-tmpl-obj-clips (object-type template slot-specs)
  (make-instance
   object-type :tmpl-name (name template)
   :slots (loop for (slot-name &key default) in (slots template)
	     collect (cons slot-name
			   (or (cpl-assoc-val slot-name slot-specs)
			       default
			       (class-slot-value object-type 'slot-default))))))

(defun make-tmpl-obj-nonclips (object-type template slot-specs)
  (make-instance
   object-type :tmpl-name (name template)
   :slots (loop for (slot-name &key default) in (slots template)
	     collect (cons slot-name
			   (or (getf slot-specs (to-keyword slot-name))
			       default
			       (class-slot-value object-type 'slot-default))))))

;; tmpl-object function searches template's slot list, finds values from them
;; in specification or falls back to default values if it finds nothing
;; if there's some other crap in specification, tmpl-object doesn't care,
;; the only condition is, that (rest specification) has to be plist
; private for package
(defun make-tmpl-object (specification object-type)
  "creates template-object of given type from its specification"
  (let* ((tmpl-name (first specification))
	 (slot-spec (rest specification))
	 (template (exil-env:find-template tmpl-name)))
    (cl:assert template () "can't find template ~A" tmpl-name)
    (if (tmpl-slot-spec-p slot-spec)
	(make-tmpl-obj-nonclips object-type template slot-spec)
	(make-tmpl-obj-clips object-type template slot-spec))))


; private
(defun clips-tmpl-slot-spec-p (specification)
  (every (lambda (slot-spec)
	   (and (listp slot-spec)
		(= (length slot-spec) 2)
		(symbolp (first slot-spec))))
	 specification))

(defun tmpl-slot-spec-p (specification)
  (every-couple (lambda (slot-name slot-val)
		  (declare (ignore slot-val))
		  (keywordp slot-name))
		specification))

; private for package
(defun tmpl-object-specification-p (specification)
  "is this a template-object specification?"
  (and (listp specification)
       (exil-env:find-template (first specification))
       (or (null (rest specification))
	   (tmpl-slot-spec-p (rest specification))
	   (clips-tmpl-slot-spec-p (rest specification)))))
