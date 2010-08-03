(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general-purpose functions

(defun variable-p (expr)
  (and (symbolp expr)
       (char-equal (char (symbol-name expr) 0) #\?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; template class

;; stores template for template facts and patterns
;; slot "slots" holds alist of slot specifiers (plists):
;; (<name> . (:default <default> [:type <type> \ planned \])
;; it is a bit redundant, since there's only one supported option
;; so far, but it's easily extensible
(defclass template ()
  ((name :reader name :initarg :name
	 :initform (error "name slot has to be specified"))
   (slots :reader slots :initarg :slots
	  :initform (error "slots slot has to be specified"))))

(defmethod tmpl-slot-spec ((template template) slot-name)
  (assoc-value slot-name (slots template)))

(defmethod tmpl-equal-p ((tmpl1 template) (tmpl2 template))
  (and (equalp (name tmpl1) (name tmpl2))
       (equalp (slots tmpl1) (slots tmpl2))))

(defmethod print-object ((tmpl template) stream)
  (print-unreadable-object (tmpl stream :type t)
    (format stream "~A ~S" (name tmpl) (slots tmpl)))
  tmpl)

;; make defclass slot-designator from the deftemplate one
(defun field->slot-designator (field)
  (destructuring-bind (name &key (default nil)) field
    `(,name . (:default ,default))))

;; creates instance of template class with given name and slot specification
;; and pushes it into *templates*.
;; it is to consider whether lambda list (name fields)
;; or (name &body fields) is better
;; for the former possibility, the call is more similar to defclass
;; for the latter, the call is more like defstruct call


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; template-object class

;; virtual template-object class, template-fact and template-object will
;; inherit from this one
;; slot slots holds alist of slot names and values
(defclass template-object ()
  ((template-name :reader tmpl-name :initarg :tmpl-name
		  ;; error would be better, but than class-slot-value
		  ;; wouldn't work, cause it doesn't provide the init value
;		  :initform (error "template-name has to be specified")
		  :initform nil)
   (slot-default :initform nil :allocation :class)
   (slots :reader slots :initarg :slots
	  :initform ())))

(defmethod tmpl-object-slot-value ((object template-object) slot-name)
  (assoc-value slot-name (slots object)))

(defmethod tmpl-object-equal-p ((object1 template-object) (object2 template-object))
  (and (equalp (tmpl-name object1) (tmpl-name object2))
       (equalp (slots object1) (slots object2))))

(defmethod print-object ((object template-object) stream)
  (if *print-escape*
      (print-unreadable-object (object stream :type t :identity t)
	(format stream "~A" (cons (tmpl-name object) (slots object))))
      (format stream "~A" (cons (tmpl-name object) (slots object))))
  object)

(defmethod find-atom (atom (object template-object))
  (find atom (mapcar #'cdr (slots object))))

(defmethod atom-postition (atom (object template-object))
  (assoc-key atom (slots object)))

;; forward declaration, real one will appear in environment.lisp
(defgeneric find-template (name &optional environment))

;; tmpl-object searches template's slot list, finds values from them in
;; specification or falls back to default values if he finds nothing
;; if there's some other crap in specification, tmpl-object doesn't care,
;; the only condition is, that (rest specification) has to be plist
(defun tmpl-object (specification object-type)
  (let ((template (find-template (first specification))))
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

(defun tmpl-object-specification-p (specification)
  (and (listp specification)
       (find-template (first specification))
       (or (null (rest specification))
	   ;; probably faster than (= (length specification) 1)
	   (keywordp (second specification)))))
