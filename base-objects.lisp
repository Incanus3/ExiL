(in-package :exil-core)

(defclass base-object () ())

; private, called by print-object
(defgeneric format-object (object stream))

(defmethod print-object ((object base-object) stream)
  (if *print-escape*
      (print-unreadable-object (object stream :type t :identity t)
        (format-object object stream))
      (format-object object stream))
  object)

(defclass simple-object (base-object)
  ((specifier :reader specifier
              :initarg :specifier)))

(defmethod exil-equal-p and ((object1 simple-object) (object2 simple-object))
  (exil-equal-p (specifier object1) (specifier object2)))

(defmethod format-object ((object simple-object) stream)
  (format stream "~S" (specifier object)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; virtual, template-fact and template-pattern will inherit from this one
;; slots holds alist of slot names and values
; private for package
(defclass template-object (base-object)
  ((template-name :reader tmpl-name
                  :initarg :tmpl-name
                  ;; error would be better, but than class-slot-value
                  ;; wouldn't work, cause it doesn't provide the init value
                  ;; :initform (error "template-name has to be specified")
                  :initform nil)
   (slots :reader slots :initarg :slots :initform ())))

; public, used in exil-env:make-tmpl-object to determine value for a slot
; whose value haven't been defined neither in the slots description, nor
; as a template default; patterns make use of this by specifying '? as default
(defgeneric slot-default (object-type)
  (:method ((type symbol)) nil))

; public
(defmethod has-slot-p ((object template-object) slot-name)
  (find slot-name (slots object) :key #'car :test #'weak-symbol-equal-p))

(defmethod exil-equal-p and ((object1 template-object)
                             (object2 template-object))
  (and (exil-weak-equal-p (tmpl-name object1) (tmpl-name object2))
       (exil-weak-equal-p (slots object1) (slots object2))))

(defmethod format-object ((object template-object) stream)
  (format stream "~A" (cons (tmpl-name object) (slots object))))

(defmethod object-slot ((object template-object) (slot-spec symbol))
  (assoc-value slot-spec (slots object) :test #'weak-symbol-equal-p))

(defmethod (setf object-slot) (val (object template-object) (slot-spec symbol))
  (unless (has-slot-p object slot-spec)
    (error "setf object-sloty: ~A doesn't have slot called ~A"
           object slot-spec))
  (setf (assoc-value slot-spec (slots object) :test #'weak-symbol-equal-p) val))

; public, used by rete
(defmethod find-atom ((object template-object) atom)
  "find the given atom in template-object slots"
  (find atom (mapcar #'cdr (slots object))))

; public, used by rete
(defmethod atom-position ((object template-object) atom)
  "get the atom position in template-object slots"
  (assoc-key atom (slots object)))
