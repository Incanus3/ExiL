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

(defmethod make-simple-fact (fact-spec)
  (make-instance 'simple-fact :fact fact-spec))

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

(defmethod fact-description ((fact simple-fact))
  (fact fact))

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

(defmethod (setf tmpl-fact-slot-value) (val (fact template-fact) slot-name)
  (setf (tmpl-object-slot-value fact slot-name) val))

; public
(defmethod fact-equal-p ((fact1 template-fact) (fact2 template-fact))
  (tmpl-object-equal-p fact1 fact2))

(defmethod fact-description ((fact template-fact))
  (cons (tmpl-name fact)
        (loop for (slot . val) in (slots fact)
           append (list (to-keyword slot) val))))

; public
(defgeneric fact-slot (fact slot-spec)
  (:documentation "returns fact's slot specified by slot-spec")
  (:method ((fact simple-fact) (slot-spec integer))
    (nth slot-spec (fact fact)))
  (:method ((fact template-fact) (slot-spec symbol))
    (tmpl-fact-slot-value fact slot-spec)))
