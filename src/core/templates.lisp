(in-package :exil-core)

;; terminology note:
;; be sure to recognize the difference between following concepts
;; a) objects - facts vs. patterns
;;    - patterns are very similar to facts, but can include variables
;;    - e.g. (on red-box blue-box)        - fact
;;    - e.g. (on red-box ?some-other-box) - pattern
;; b) simple vs. template objects
;;    - object slots can be identified either by position - simple objects
;;      or by name - template objects
;;    - e.g. (on red-box blue-box)       - simple fact
;;    - e.g. (on red-box ?other-box)     - simple pattern
;;    - e.g. (car :color red :mph 160)   - template-fact
;;      equal to (car :mph 160 :color red)
;;    - e.g. (car :color ?clr :mph ?mph) - template-pattern
;; c) templates - instances of class template
;;    - prescriptions for template objects - describe slots and default values

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; public interface:
;; (defclass template () (name slots))
;; (defun make-template (name slots))
;; (defmacro doslots ((name default template &optional retval) &body body))

(defgeneric exil-equal-p (obj1 obj2)
  (:documentation "ExiL default equality predicate")
  (:method (obj1 obj2) nil))

(defgeneric external (obj)
  (:documentation "Constructs an external representation of given object")
  (:method (null) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stores template for template facts and patterns
;; slot "slots" holds alist of slot specifiers (plists):
;; (<name> . (:default <default> [:type <type> \ planned \]))
;; it is a bit redundant, since there's only one supported option
;; so far, but it's easily extensible
;; public
(defclass template ()
  ((name :reader name :initarg :name
         :initform (error "name has to be specified"))
   (slots :reader slots :initarg :slots
          :initform (error "slots have to be specified"))))

;; public
(defmethod external ((tmpl template))
  (list (name tmpl) (copy-tree (slots tmpl))))

;; public
(defmethod exil-equal-p ((tmpl1 template) (tmpl2 template))
  (and (equalp (name tmpl1) (name tmpl2))
       (equalp (slots tmpl1) (slots tmpl2))))

;; public
(defmethod print-object ((tmpl template) stream)
  (if *print-escape*
      (print-unreadable-object (tmpl stream :type t)
	(format stream "~A ~A" (name tmpl) (slots tmpl)))
      (format stream "(TEMPLATE ~A~%  ~A)" (name tmpl) (slots tmpl)))
  tmpl)

;; make-template ensures that slot-names are keywords
;; public
(defun make-template (name slots)
  "template constructor"
  (make-instance 'template :name (to-keyword name)
                 :slots (mapcar (lambda (slot-spec)
                                  (cons (to-keyword (car slot-spec))
                                        (cdr slot-spec)))
                                (to-list-of-lists slots))))

;; iterates over template's slots, introducing variables (whose names are
;; given by name and default) in the body, hiding actual slots representation
;; public
(defmacro doslots ((name default template &optional retval) &body body)
  "destructuring iteration macro"
  (let ((slot (gensym "slot")))
    `(progn
       (dolist (,slot (slots ,template))
         (destructuring-bind (,name &key ((:default ,default))) ,slot
           ,@body))
       ,retval)))
