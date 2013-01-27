(in-package :exil-core)

(defclass base-object () ())

(defclass simple-object (base-object)
  ((specifier :reader specifier)))

(defmethod exil-equal-p and ((object1 simple-object) (object2 simple-object))
  (exil-equal-p (specifier object1) (specifier object2)))

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

; private
(defmethod tmpl-object-slot-value ((object template-object) slot-name)
  "get the template-object slot value according to the slot name"
  (assoc-value slot-name (slots object) :test #'weak-symbol-equal-p))

; private
(defmethod (setf tmpl-object-slot-value) (val (object template-object) slot-name)
  (unless (has-slot-p object slot-name)
    (error "setf tmpl-object-slot-value: ~A doesn't have slot called ~A"
           object slot-name))
  (setf (assoc-value slot-name (slots object) :test #'weak-symbol-equal-p) val))

(defmethod exil-equal-p and ((object1 template-object)
                             (object2 template-object))
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

; public, used by rete
(defmethod find-atom ((object template-object) atom)
  "find the given atom in template-object slots"
  (find atom (mapcar #'cdr (slots object))))

; public, used by rete
(defmethod atom-position ((object template-object) atom)
  "get the atom position in template-object slots"
  (assoc-key atom (slots object)))
