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

(defmacro make-fact (fact)
  `(make-instance 'simple-fact :fact ',fact))

;; prints facts
(defmethod print-object ((fact simple-fact) stream)
  (print-unreadable-object (fact stream :type t)
    (format stream "~s" (fact fact))
    fact))

(defmethod fact-equal-p ((fact1 simple-fact) (fact2 simple-fact))
  (equalp (fact fact1) (fact fact2)))

;; stores template for template facts
;; slot slots holds alist of slot specifiers (plists):
;; (<name> . (:default <default> [:type <type> \ planned \])
(defclass template ()
  ((name :reader name :initarg :name
	 :initform (error "name slot has to be specified"))
   (slots :reader slots :initarg :slots
	  :initform (error "slots slot has to be specified"))))

(defun add-template (template &optional (environment *current-environment*))
  (setf (gethash (name template) (templates environment)) template)
  template)

(defun find-template (name &optional (environment *current-environment*))
  (gethash name (templates environment)))

(defmethod tmpl-slot-spec ((template template) slot-name)
  (assoc-value slot-name (slots template)))

(defmethod tmpl-equal-p ((tmpl1 template) (tmpl2 template))
  (and (equalp (name tmpl1) (name tmpl2))
       (equalp (slots tmpl1) (slots tmpl2))))

(defmethod print-object ((tmpl template) stream)
  (format stream "~A" `(template
			,(name tmpl)
			,(slots tmpl))))

;; make defclass slot-designator from the deftemplate one
(defun field->slot-designator (field)
  (destructuring-bind (field name &key (default nil)) field
    (declare (ignore field))
    `(,name . (:default ,default))))

;; creates instance of template class with given name and slot specification
;; and pushes it into *templates*.
(defmacro deftemplate (name &body fields)
  (let ((template (gensym "template")))
    `(let ((,template
	    (make-instance
	     'template
	     :name ',name
	     :slots ',(loop for field in fields
			 collect (field->slot-designator field)))))
       (add-template ,template))))

;; stores template fact
;; slot slots holds alist of slot names and values
(defclass template-fact (fact)
  ((template-name :reader tmpl-name :initarg :tmpl-name
		  :initform (error "template-name slot has to be specified"))
   (slots :reader slots :initarg :slots
	  :initform (error "slots slot has to be specified"))))

(defmacro tmpl-fact (fact-spec)
  (let ((template (find-template (first fact-spec))))
    (cl:assert template () "can't find template ~A" (first fact-spec))
    `(make-instance
      'template-fact
      :tmpl-name ',(first fact-spec)
      :slots ',(loop
		 with initargs = (rest fact-spec)
		 for slot-spec in (slots template)
		 collect (cons (car slot-spec)
			       (or (getf initargs
					 (to-keyword (car slot-spec)))
				   (getf (cdr slot-spec)
					 :default)))))))

(defun tmpl-fact-p (fact-spec)
  (find-template (first fact-spec)))

(defmethod tmpl-fact-slot-value ((fact template-fact) slot-name)
  (assoc-value slot-name (slots fact)))

(defmethod fact-equal-p ((fact1 template-fact) (fact2 template-fact))
  (equalp (slots fact1) (slots fact2)))

(defmethod print-object ((fact template-fact) stream)
  (print-unreadable-object (fact stream :type t)
    (format stream "~A" (cons (tmpl-name fact) (slots fact)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; application macros

(defmacro assert (fact-spec &optional (environment *current-environment*))
  "Add fact into working memory"
  (if (tmpl-fact-p fact-spec)
      `(pushnew (tmpl-fact ,fact-spec) (facts environment) :test #'fact-equal-p)
      `(pushnew (make-instance 'simple-fact :fact ',fact-spec) (facts environment)
	       :test #'fact-equal-p)))

(defmacro retract (fact)
  "Remove fact from working memory"
  (declare (ignorable fact))
  )

(defmacro deffacts (facts-list)
  "Create group of facts to be asserted after (reset)"
  (declare (ignorable facts-list))
  )