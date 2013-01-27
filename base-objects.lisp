(in-package :exil-core)

(defclass base-object () ())

; private, called by print-object
(defgeneric exil-equal-p (obj1 obj2)
  (:documentation "ExiL default equality predicate")
  (:method-combination and))
(defgeneric format-object (object stream))
(defgeneric print-object (object stream))
(defgeneric copy-object (object))
(defgeneric object-slot (object slot-spec))
(defgeneric (setf object-slot) (val object slot-spec))
(defgeneric find-atom (object atom))
(defgeneric atom-position (object atom))
(defgeneric description (object))

(defmethod print-object ((object base-object) stream)
;  (if *print-escape*
      (print-unreadable-object (object stream :type t :identity t)
        (format-object object stream))
;      (format-object object stream))
  object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass simple-object (base-object)
  ((specifier :reader specifier
              :initarg :specifier)))

(defmethod exil-equal-p and ((object1 simple-object) (object2 simple-object))
  (weak-equal-p (specifier object1) (specifier object2)))

(defmethod format-object ((object simple-object) stream)
  (format stream "~S" (specifier object)))

(defmethod copy-object ((object simple-object))
  (make-instance (class-of object) :specifier (copy-list (specifier object))))

(defmethod object-slot ((object simple-object) (slot-spec integer))
  (nth slot-spec (specifier object)))

(defmethod (setf object-slot) (val (object simple-object) (slot-spec integer))
  (setf (nth slot-spec (specifier object)) val))

; public, used by rete
(defmethod find-atom ((object simple-object) atom)
  (find atom (specifier object)))

; public, used by rete
(defmethod atom-position ((object simple-object) atom)
  (position atom (specifier object)))

(defmethod description ((object simple-object))
  (specifier object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; virtual, template-fact and template-pattern will inherit from this one
;; slots holds alist of slot names and values
; private for package
(defclass template-object (base-object)
  ((template-name :reader tmpl-name
                  :initarg :tmpl-name
                  :initform (error "template-name has to be specified"))
;                  :initform nil)
   (slots :reader slots :initarg :slots :initform ())))

(defgeneric has-slot-p (tmpl-object slot-name))

(defmethod exil-equal-p and ((object1 template-object)
                             (object2 template-object))
  (and (weak-equal-p (tmpl-name object1) (tmpl-name object2))
       (weak-equal-p (slots object1) (slots object2))))

(defmethod format-object ((object template-object) stream)
  (format stream "~A" (cons (tmpl-name object) (slots object))))

(defmethod copy-object ((object template-object))
  (make-instance (class-of object)
                 :tmpl-name (tmpl-name object)
                 :slots (copy-alist (slots object))))

; public
(defmethod has-slot-p ((object template-object) slot-name)
  (find slot-name (slots object) :key #'car :test #'weak-equal-p))

(defmethod object-slot ((object template-object) (slot-spec symbol))
  (assoc-value slot-spec (slots object) :test #'weak-equal-p))

(defmethod (setf object-slot) (val (object template-object) (slot-spec symbol))
  (unless (has-slot-p object slot-spec)
    (error "setf object-sloty: ~A doesn't have slot called ~A"
           object slot-spec))
  (setf (assoc-value slot-spec (slots object) :test #'weak-equal-p) val))

; public, used by rete
(defmethod find-atom ((object template-object) atom)
  "find the given atom in template-object slots"
  (find atom (mapcar #'cdr (slots object))))

; public, used by rete
(defmethod atom-position ((object template-object) atom)
  "get the atom position in template-object slots"
  (assoc-key atom (slots object)))

; public
(defmethod description ((object template-object))
  (cons (tmpl-name object)
        (loop for (slot . val) in (slots object)
           append (list (to-keyword slot) val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public, used in exil-env:make-tmpl-object to determine value for a slot
; whose value haven't been defined neither in the slots description, nor
; as a template default; patterns make use of this by specifying '? as default
(defgeneric slot-default (object-type)
  (:method ((type symbol)) nil))
