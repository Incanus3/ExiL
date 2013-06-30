(in-package :exil-core)

; private
(defclass base-object () ())

; private
(defgeneric format-object (object stream))
; public
(defgeneric copy-object (object))
; public
(defgeneric object-slot (object slot-spec))
(defgeneric (setf object-slot) (val object slot-spec))
; public
(defgeneric atom-position (object atom))
; public
(defgeneric description (object))

(defmethod print-object ((object base-object) stream)
  (if *print-escape*
      (print-unreadable-object (object stream :type t :identity nil)
        (format-object object stream))
      (format-object object stream))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; private
(defclass simple-object (base-object)
  ((specifier :reader specifier
              :initarg :specifier)))

(defmethod exil-equal-p ((object1 simple-object) (object2 simple-object))
  (weak-equal-p (specifier object1) (specifier object2)))

(defmethod format-object ((object simple-object) stream)
  (format stream "~A" (specifier object)))

(defmethod copy-object ((object simple-object))
  (make-instance (class-of object) :specifier (copy-list (specifier object))))

(defmethod object-slot ((object simple-object) (slot-spec integer))
  (nth slot-spec (specifier object)))

(defmethod (setf object-slot) (val (object simple-object) (slot-spec integer))
  (setf (nth slot-spec (specifier object)) val))

; public, used by rete
(defmethod atom-position ((object simple-object) atom)
  (position atom (specifier object)))

(defmethod description ((object simple-object))
  (specifier object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; virtual, template-fact and template-pattern will inherit from this one
;; template-name is a keyword
;; slots holds alist of slot names and values
;; slot names are keyword symbols
; private for package
(defclass template-object (base-object)
  ((template :reader template
             :initarg :template
             :initform (error "template has to be specified"))
   (slots :accessor slots :initarg :slots :initform ())))

(defgeneric template-name (object))
; public
(defmethod template-name ((object template-object))
  (name (template object)))

(defmethod exil-equal-p ((object1 template-object)
                         (object2 template-object))
  (and (exil-equal-p (template object1) (template object2))
       (alist-equal-p (slots object1) (slots object2))))

;; TODO: change this so it corresponds to assert format
(defmethod format-object ((object template-object) stream)
  (format stream "~A" (cons (name (template object)) (slots object))))

(defmethod copy-object ((object template-object))
  (make-instance (class-of object)
                 :template (template object)
                 :slots (copy-alist (slots object))))

(defmethod object-slot ((object template-object) (slot-name symbol))
  (assoc-value slot-name (slots object)))

(defmethod (setf object-slot) (val (object template-object) (slot-name symbol))
    (setf (assoc-value slot-name (slots object)) val))

; public, used by rete
(defmethod atom-position ((object template-object) atom)
  "get the atom position in template-object slots"
  (assoc-key atom (slots object)))

; public
(defmethod description ((object template-object))
  (cons (name (template object))
        (iter (for (slot . val) :in (slots object))
              (appending (list (to-keyword slot) val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; used to determine value for a slot whose value haven't been defined neither
;; in the slots description, nor as a template default
;; patterns make use of this by specifying '? as default
; private
(defgeneric slot-default (object-type)
  (:method ((type symbol)) nil))

; private
(defgeneric make-tmpl-object (template slot-spec obj-type)
  (:documentation "finds values for slots in slot-spec, template defaults or
                   global slot-default, creates new object with those slots"))

;; slot-spec is a plist mapping slot-names to values
;; obj-type is either template-fact or template-pattern
(defmethod make-tmpl-object ((tmpl template) (slot-spec list) (obj-type symbol))
  (let (slots)
    (doslots (slot-name default tmpl)
      (push-end (cons slot-name
                  (or (getf slot-spec slot-name)
                      default
                      (slot-default obj-type)))
            slots))
    (make-instance obj-type
                   :template tmpl :slots slots)))
