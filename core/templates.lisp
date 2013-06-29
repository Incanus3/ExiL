(in-package :exil-core)

;(defclass template () (name slots))
(defgeneric exil-equal-p (obj1 obj2)
  (:documentation "ExiL default equality predicate")
  (:method (obj1 obj2) nil))
;(defun make-template (name slots))
;(defmacro doslots ((name default template &optional retval) &body body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; template class

;; stores template for template facts and patterns
;; slot "slots" holds alist of slot specifiers (plists):
;; (<name> . (:default <default> [:type <type> \ planned \]))
;; it is a bit redundant, since there's only one supported option
;; so far, but it's easily extensible
; public
(defclass template ()
  ((name :reader name :initarg :name
         :initform (error "name has to be specified"))
   (slots :reader slots :initarg :slots
          :initform (error "slots have to be specified"))))

; public
(defmethod exil-equal-p ((tmpl1 template) (tmpl2 template))
  (equalp (name tmpl1) (name tmpl2)))

; public
(defmethod print-object ((tmpl template) stream)
  (print-unreadable-object (tmpl stream :type t)
    (format stream "~A ~A" (name tmpl) (slots tmpl)))
  tmpl)

;; make-template ensures that slot-names are keywords
; public
(defun make-template (name slots)
  (make-instance 'template :name (to-keyword name)
                 :slots (mapcar (lambda (slot-spec)
                                  (cons (to-keyword (car slot-spec))
                                        (cdr slot-spec)))
                                (to-list-of-lists slots))))

; iterates over template's slots, introducing variables (whose names are
; given by name and default) in the body
; public
(defmacro doslots ((name default template &optional retval) &body body)
  (let ((slot (gensym "slot")))
    `(progn
       (dolist (,slot (slots ,template))
         (destructuring-bind (,name &key ((:default ,default))) ,slot
           ,@body))
       ,retval)))
