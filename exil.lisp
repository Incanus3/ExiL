(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global variables

(defvar *facts* nil)
(defvar *templates* nil)
(defvar *rules* nil)

(defun to-keyword (symbol)
  "get keyword form of symbol"
  (intern (symbol-name symbol) :keyword))

;; concatenate "tmpl-" before symbol.
;; doesn't work as i'd like: returns |tmpl-<symbol>|. ask smb less dumb
(defun symbol->tmpl-symbol (symbol &optional (pkg :exil))
  (intern (concatenate 'string "tmpl-" (symbol-name symbol)) pkg))

;; make defclass slot-designator from the deftemplate one
(defun field->slot-designator (field)
  (destructuring-bind (field name &key (default nil)
                             (type t type-provided-p)) field
    (declare (ignore field))
    `(,name :initarg ,(to-keyword name)
            :initform ,default
;			:accessor ,(symbol->tmpl-symbol name)
			:accessor ,name
            ,@(when type-provided-p `(:type ,type)))))

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

;; prints facts
(defmethod print-object ((fact simple-fact) stream)
  (print-unreadable-object (fact stream)
    (format stream "simple-fact ~s"
	    (fact fact))
    fact))

(defmethod fact-equal-p ((fact1 simple-fact) (fact2 simple-fact))
  (equalp (fact fact1) (fact fact2)))

;; virtual class template
(defclass template (fact)
  ((fields :reader fields)))

;; prints template
(defmethod print-object ((fact template) stream)
  (print-unreadable-object (fact stream)
    (format stream "template-fact ~s"
	    `(,(type-of fact)
	       ,@(loop for field in (fields fact)
		    collect (to-keyword field)
		    collect (slot-value fact field)))))
  fact)

;; Defines class with the same name as template and with slot for every
;; field and one additional slot for list of field-slots,
;; then pushes template name into *templates*.
;; This is necessary for defrule to be able to distinguish between normal
;; and template facts.
(defmacro deftemplate (name &body fields)
  "Define fact template"
  `(progn
     (defclass ,name (template)
       (,@(loop for field in (car fields)
	     collect (field->slot-designator field))
	(fields :initform ',(mapcar #'second (car fields)) :reader fields)))
     
     (defun ,name (&rest rest)
       (apply #'make-instance ',name rest))
     
     (defmethod fact-equal-p ((obj1 ,name) (obj2 ,name))
       (and
	,@(loop for field in (car fields)
	     collect `(equalp (slot-value obj1 ',(second field))
			      (slot-value obj2 ',(second field))))))
     
     (pushnew ',name *templates*)

     ;; could have remembered return-value of 1.st expression and return it
     ;; but hope this is more effective
     (find-class ',name)))

(defmacro assert (fact)
  "Add fact into working memory"
  (if (find (first fact) *templates*)
      `(pushnew ,fact *facts* :test fact-equal-p)
      `(pushnew (make-instance 'simple-fact :fact ',fact) *facts*
	       :test #'fact-equal-p)))

(defmacro retract (fact)
  "Remove fact from working memory"
  (declare (ignorable fact))
  )

(defmacro deffacts (facts-list)
  "Create group of facts to be asserted after (reset)"
  (declare (ignorable facts-list))
  )

(defclass rule ()
  ((lhs :initarg :lhs :reader lhs)
   (rhs :initarg :rhs :reader rhs)))

(defmacro defrule (&body rule)
  "Define rule"
  (let ((=>-position (position '=> rule))
	(rule (gensym)))
    `(let ((,rule (make-instance
		   'rule
		   :lhs ',(subseq rule 0 =>-position)
		   :rhs ',(subseq rule (1+ =>-position)))))
       

(defun reset ()
  "Reset the environment"

  )

(defun run ()
  "Run the infenece engine"

  )
