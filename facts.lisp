(in-package :exil-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fact classes

;; virtual class fact
; public
(defclass fact () ())

;; fact equality predicate
; public
(defgeneric fact-equal-p (fact1 fact2)
  (:method (fact1 fact2) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; class simple-fact
; public, used by rete
(defclass simple-fact (fact)
  ((fact :initform (error "Fact slot must be specified")
	 :initarg :fact
	 :reader fact)))

; private
(defmethod initialize-instance :after ((simple-fact simple-fact) &key)
  (cl:assert (notany #'variable-p (fact simple-fact))
	     () "fact can't include variables"))

;; prints facts
; public
(defmethod print-object ((fact simple-fact) stream)
  (if *print-escape*
      (print-unreadable-object (fact stream :type t :identity t)
	(format stream "~s" (fact fact)))
      (format stream "~s" (fact fact)))
  fact)

; public
(defmethod fact-equal-p ((fact1 simple-fact) (fact2 simple-fact))
  (equalp (fact fact1) (fact fact2)))

; public, used by rete
(defmethod find-atom ((fact simple-fact) atom)
  (find atom (fact fact)))

; public, used by rete
(defmethod atom-position ((fact simple-fact) atom)
  (position atom (fact fact)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stores template fact
;; slot slots holds alist of slot names and values
; public
(defclass template-fact (fact template-object) ())

; private
(defmethod initialize-instance :after ((fact template-fact) &key)
  (cl:assert (notany #'variable-p (mapcar #'cdr (slots fact)))
	     () "fact can't include variables"))

; public, used by rete
(defmethod tmpl-fact-slot-value ((fact template-fact) slot-name)
  (tmpl-object-slot-value fact slot-name))

; public
(defmethod fact-equal-p ((fact1 template-fact) (fact2 template-fact))
  (tmpl-object-equal-p fact1 fact2))

; public
(defgeneric fact-slot (fact slot-spec)
  (:documentation "returns fact's slot specified by slot-spec")
  (:method ((fact simple-fact) (slot-spec integer))
    (nth slot-spec (fact fact)))
  (:method ((fact template-fact) (slot-spec symbol))
    (tmpl-fact-slot-value fact slot-spec)))

;; tmpl-fact searches template's slot list, finds values from them in
;; fact-spec or falls back to default values if he finds nothing
;; if there's some other crap in fact-spec, tmpl-fact doesn't care,
;; the only condition is, that (rest fact-spec) has to be plist
; private
(defun make-tmpl-fact (fact-spec)
  (make-tmpl-object fact-spec 'template-fact))

; private for package
(defun tmpl-fact-specification-p (fact-spec)
  (tmpl-object-specification-p fact-spec))

; public
(defun make-fact (fact-spec)
  (if (tmpl-fact-specification-p fact-spec)
      (make-tmpl-fact fact-spec)
      (make-instance 'simple-fact :fact fact-spec)))

