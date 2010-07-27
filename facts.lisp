(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fact classes

;; virtual class fact
(defclass fact () ())

;; fact equality predicate
(defgeneric fact-equal-p (fact1 fact2)
  (:method (fact1 fact2) nil))

;; class simple-fact
(defclass simple-fact (fact)
  ((fact :initform (error "Fact slot must be specified")
	 :initarg :fact
	 :reader fact)))

(defmethod initialize-instance :after ((simple-fact simple-fact) &key)
  (cl:assert (notany #'variable-p (fact simple-fact))
	     () "fact can't include variables"))

;; prints facts
(defmethod print-object ((fact simple-fact) stream)
  (print-unreadable-object (fact stream :type t :identity t)
    (format stream "~s" (fact fact))
    fact))

(defmethod fact-equal-p ((fact1 simple-fact) (fact2 simple-fact))
  (equalp (fact fact1) (fact fact2)))

(defmethod find-atom (atom (fact simple-fact))
  (find atom (fact fact)))

(defmethod atom-postition (atom (fact simple-fact))
  (position atom (fact fact)))

;; stores template fact
;; slot slots holds alist of slot names and values
(defclass template-fact (fact template-object) ())


(defmethod initialize-instance :after ((fact template-fact) &key)
  (cl:assert (notany #'variable-p (mapcar #'cdr (slots fact)))
	     () "fact can't include variables"))


(defmethod tmpl-fact-slot-value ((fact template-fact) slot-name)
  (tmpl-object-slot-value fact slot-name))

(defmethod fact-equal-p ((fact1 template-fact) (fact2 template-fact))
  (tmpl-object-equal-p fact1 fact2))

(defgeneric fact-field (fact field)
  (:documentation "returns fact's field")
  (:method ((fact simple-fact) (field integer))
    (nth field (fact fact)))
  (:method ((fact template-fact) (field symbol))
    (tmpl-fact-slot-value fact field)))

