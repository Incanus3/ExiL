(in-package :exil-core)

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
             () "fact can't include variables"))

(defun make-simple-fact (fact-spec)
  (make-instance 'simple-fact :fact (copy-list fact-spec)))

;;;; inherited from simple-object:
;; exil-equal-p, format-object, print-object, copy-object, object-slot,
;; find-atom, atom-position, description

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slots (inherited from template-object) holds alist of slot names and values
; public
(defclass template-fact (fact template-object) ())

; private
(defmethod initialize-instance :after ((fact template-fact) &key)
  (cl:assert (notany #'variable-p (mapcar #'cdr (slots fact)))
             () "fact can't include variables"))

;;;; inherited from template-object:
;; exil-equal-p, format-object, print-object, copy-object, object-slot,
;; find-atom, atom-position, description, has-slot-p
