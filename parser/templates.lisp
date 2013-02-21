(in-package :exil-parser)

;; TODO: parsing of the template is a responsibility of parser, deftemplate
;;   should just pass the name and slots to parser and add the result to env
;; TODO: use defaults in examples to test this

;; is this a valid exil slot specifier?
; private
(defun nonclips-slot-spec-p (slot-spec)
  (and (symbolp (first slot-spec))
       (plistp (rest slot-spec))))

;; is this a clips deftemplate syntax slot specifier?
; private
(defun clips-slot-spec-p (slot-spec)
  (and (equalp (to-keyword (first slot-spec)) :slot))
  (symbolp (second slot-spec))
  (listp (nthcdr 2 slot-spec)))

;; convert clips deftemplate syntax slot specifier to exil syntax
; private
(defun clips-slot->slot-spec (slot-spec)
  (destructuring-bind (slot slot-name &optional modifiers) slot-spec
    (declare (ignore slot))
    `(,slot-name . (:default ,(second modifiers)))))

;; ensure slot-spec is a valid exil slot-specifier
; private
(defun slot->slot-specifier (slot-spec)
  (cond
    ((nonclips-slot-spec-p slot-spec) slot-spec)
    ((clips-slot-spec-p slot-spec) (clips-slot->slot-spec slot-spec))
    (t (error "~A not a valid template slot specifier~%" slot-spec))))

;; ensure deftemplate slot specifiers are valid exil specifiers
; private
(defun slots->slot-specifiers (slots)
  (mapcar #'slot->slot-specifier (to-list-of-lists slots)))

(defun parse-template (name slots)
  (make-template name (slots->slot-specifiers slots)))
