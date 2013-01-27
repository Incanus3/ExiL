(in-package :exil-core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; public, virtual
(defclass fact () ())

(defgeneric fact-description (fact))
(defgeneric copy-fact (fact))
(defgeneric fact-slot (fact slot-spec))

;; needed e.g. in tokens:includes-p which calls (exil-equal-p fact (wme token))
;; where for empty-token (wme token) is nil
(defmethod exil-equal-p and ((fact fact) (null null))
  nil)

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

;; prints facts
; public
(defmethod print-object ((fact simple-fact) stream)
  (labels ((print-fact () (format stream "~S" (fact fact))))
    (if *print-escape*
        (print-unreadable-object (fact stream :type t :identity t)
          (print-fact))
        (print-fact)))
  fact)

(defmethod fact-description ((fact simple-fact))
  (fact fact))

(defmethod fact-slot ((fact simple-fact) (slot-spec integer))
  (nth slot-spec (fact fact)))

(defmethod (setf fact-slot) (val (fact simple-fact) (slot-spec integer))
  (setf (nth slot-spec (fact fact)) val))

(defmethod copy-fact ((fact simple-fact))
  (make-simple-fact (fact fact)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slots (inherited from template-object) holds alist of slot names and values
; public
(defclass template-fact (fact template-object) ())

; private
(defmethod initialize-instance :after ((fact template-fact) &key)
  (cl:assert (notany #'variable-p (mapcar #'cdr (slots fact)))
             () "fact can't include variables"))

;; find-atom and atom-position inherited from template-object

; public
(defmethod fact-description ((fact template-fact))
  (cons (tmpl-name fact)
        (loop for (slot . val) in (slots fact)
           append (list (to-keyword slot) val))))

; public
(defmethod fact-slot ((fact template-fact) (slot-spec symbol))
  (tmpl-object-slot-value fact slot-spec))

(defmethod (setf fact-slot) (val (fact template-fact) (slot-spec symbol))
  (setf (tmpl-object-slot-value fact slot-spec) val))

; public, used by exil-env:modify-fact
(defmethod copy-fact ((fact template-fact))
  (make-instance 'template-fact
                 :tmpl-name (tmpl-name fact)
                 :slots (copy-alist (slots fact))))
