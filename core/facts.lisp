(in-package :exil-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Facts represent the expert system's knowledge of the world, they can be
;; simple (represented by a simple list of atoms - typically symbols and numbers)
;; or templated. Each template-fact is associated with a particular template,
;; that determines names (symbols) and default values of its slots.
;; Creating of template-facts from user-given slot specification and default
;; values stored in the template is done by make-fact function defined in
;; the environment package, as it has to have access to the templates defined
;; in the environment.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defclass fact () ())
;(defclass simple-fact (fact simple-object) (specifier))
;(defun make-simple-fact (fact-spec))
;(defclass template-fact (fact template-object) ())
;(defgeneric make-template-fact (template slot-spec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public, virtual
(defclass fact () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public, used by rete
(defclass simple-fact (fact simple-object)
  ((specifier :initform (error "Fact slot must be specified")
              :initarg :fact
              :reader fact)))

; private
(defmethod initialize-instance :after ((simple-fact simple-fact) &key)
  (cl:assert (notany #'variable-p (fact simple-fact))
             () "fact must not include variables"))

; public
(defun make-simple-fact (fact-spec)
  (make-instance 'simple-fact :fact (copy-list fact-spec)))

;;;; inherited from simple-object:
;; exil-equal-p, format-object, print-object, copy-object, object-slot,
;; find-atom, atom-position, description

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slots (inherited from template-object) holds alist of slot names and values
; public
(defclass template-fact (fact template-object) ())

(defgeneric alter-fact (fact mod-list))

; private
(defmethod initialize-instance :after ((fact template-fact) &key)
;  (cl:assert (notany #'variable-p (mapcar #'cdr (slots fact)))
;             () "fact must not include variables")
  )

; public
(defgeneric make-template-fact (template slot-spec)
  (:documentation "finds values for fact's slots, create new template-fact"))

(defmethod make-template-fact ((tmpl template) (slot-spec list))
  (make-tmpl-object tmpl slot-spec 'template-fact))

;; mod-list is a plist mapping slot-name to new value
(defmethod alter-fact ((fact template-fact) (mod-list list))
  "returns new fact that differs from fact by application of mod-list"
  (let ((new-fact (copy-object fact)))
    (doplist (slot-name val mod-list)
      (setf (object-slot new-fact slot-name) val))
    new-fact))

;;;; inherited from template-object:
;; exil-equal-p, format-object, print-object, copy-object, object-slot,
;; find-atom, atom-position, description, has-slot-p
