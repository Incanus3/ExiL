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
;; (<name> (:default <default> [:type <type> \ planned \]))
;; it is a bit redundant, since there's only one supported option
;; so far, but it's easily extensible
; public
(defclass template ()
  ((name :reader name :initarg :name
         :initform (error "name has to be specified"))
   (slots :reader slots :initarg :slots
          :initform (error "slots have to be specified"))))

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
  (make-instance 'template :name name :slots (to-list-of-lists slots)))

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
