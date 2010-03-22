(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pattern classes

(defclass pattern (fact) ())

(defclass simple-pattern (pattern simple-fact) ((fact :initarg :pattern
						      :reader pattern)))

(defmacro make-pattern (pattern)
  `(make-instance 'simple-pattern :pattern ',pattern))

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

;; could use hash-table instead of assoc-list, if the facts has many atoms
;; in need of longer facts could be 2 methods (with assoc-list and hash table)
;; and one that would call them depending on length
(defmethod variable-bindings ((pattern simple-pattern)
			      (fact simple-fact))
  (let ((bindings))
    (mapcar (lambda (pt-atom atom)
	      (if (variable-p pt-atom)
		  (let ((binding (cdr (assoc pt-atom bindings))))
		    (if binding
			(unless (atom-equal-p binding atom)
			  (return-from variable-bindings nil))
			(push (cons pt-atom atom) bindings)))
		  (unless (atom-equal-p pt-atom atom)
		    (return-from variable-bindings nil))))
	    (pattern pattern)
	    (fact fact))
    (nreverse bindings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rete classes

(defclass node () ((children :accessor childern :initform nil)))

(defclass alpha-test-node (node)
  ((pattern :reader pattern
	    :initarg :pattern
	    :initform (error "alpha-test-node pattern has to be specified")))

(defclass alpha-memory-node (node) ((fact-bindings-pairs :accessor fb-pairs)))

(defclass beta-join-node (node) ())

(defclass beta-memory-node (node) ((tokens :accessor tokens)))

(defclass rete () ((alpha-test-nodes   :accessor a-test-nodes)
		   (alpha-memory-nodes :accessor a-mem-nodes)
		   (beta-join-nodes    :accessor b-join-nodes
				       :initform (make-instance 'beta-join-node))
		   (beta-memory-nodes  :accessor b-mem-nodes)))

(defmethod add-rule ((rete rete))
  
  )
