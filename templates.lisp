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
  (assoc-value slot-name (slots object)))

; private for package
(defmethod tmpl-object-equal-p ((object1 template-object) (object2 template-object))
  "template-object equality predicate"
  (and (equalp (tmpl-name object1) (tmpl-name object2))
       (equalp (slots object1) (slots object2))))

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

;; forward declaration, real one will appear in environment.lisp
; private for package, forward declaration of environment:find-template
;(defgeneric find-template (name))

;; tmpl-object function searches template's slot list, finds values from them
;; in specification or falls back to default values if it finds nothing
;; if there's some other crap in specification, tmpl-object doesn't care,
;; the only condition is, that (rest specification) has to be plist
; private for package
(defun make-tmpl-object (specification object-type)
  "creates template-object of given type from its specification"
  (let ((template (exil-env:find-template (first specification))))
    (cl:assert template () "can't find template ~A" (first specification))
    (make-instance
     object-type ;; >>>>>>>>>>>>>> cat's standing on my keyboard
     :tmpl-name (first specification)
     :slots (loop with initargs = (rest specification)
		 for slot-spec in (slots template)
		 collect (cons (car slot-spec)
			       (or (getf initargs
					 (to-keyword (car slot-spec)))
				   (getf (cdr slot-spec)
					 :default)
				   (class-slot-value object-type 'slot-default)))))))

; private for package
(defun tmpl-object-specification-p (specification)
  "is this a template-object specification?"
  (and (listp specification)
       (exil-env:find-template (first specification))
       (or (null (rest specification))
	   ;; probably faster than (= (length specification) 1)
	   (keywordp (second specification)))))
