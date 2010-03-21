(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pattern classes

(defclass pattern (fact) ())

(defclass simple-pattern (pattern simple-fact) ((fact :initarg :pattern
						      :reader pattern)))

(defclass template-pattern (pattern template) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fact matching

(defgeneric atom-equal-p (object1 object2)
  (:documentation "equality predicate for fact atoms")
  (:method (object1 object2) (equalp object1 object2)))

;; terminology note:
;; be sure about distiction of following 2 things
;; a) templates - instances of class template
;;      - they're just facts with named slots
;;      - e.g. (car (color red) (mph 160))
;; b) patterns - could be either in simple fact or in template fact form
;;      - they're not facts, they can include wildcards, variables, etc.
;;      - e.g. (on red-box ?some-other-box)
;;      - or (car (color ?some-color))

(defgeneric variable-bindings (pattern fact)
  (:documentation "if the fact passes the pattern,
                     returns the variable bindings in assoc list"))

(defun variable-p (symbol)
  (char-equal (char (symbol-name symbol) 0) #\?))

(defmethod variable-bindings ((pattern simple-pattern)
			      (fact simple-fact))
  (let ((bindings))
    (mapcar (lambda (pt-atom atom)
	      (if (variable-p pt-atom)
		  (let ((binding (assoc pt-atom bindings)))
		    (if binding
			(unless (atom-equal-p binding atom)
			  (return-from variable-bindings nil))
			(push (cons pt-atom atom) bindings)))
		  (unless (atom-equal-p pt-atom atom)
		    (return-from variable-bindings nil))))
	    (pattern pattern)
	    (fact fact))
    bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rete class

(defclass rete () ((working-memory :accessor wm)
		   (production-memory :accessor pm)))

(defmethod add-rule ((rete rete))
  
  )