(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass exil-environment ()
  ((facts :initform ())
   (fact-groups :initform ())
   (templates :initform (make-hash-table :test 'equalp))
   (rules :initform (make-hash-table :test 'equalp))
   (rete :initform (make-instance 'rete))))

(defvar *environments*
  (let ((table (make-hash-table)))
    (setf (gethash "default" table)
	  (make-instance 'exil-environment))
    table))

(defvar *current-environment*
  (gethash "default" *environments*))

(defmacro defenv (name &key (redefine nil))
  (let ((name (symbol-name name)))
    (when (or (not (gethash name *environments*))
	      redefine)
      (setf (gethash name *environments*)
	    (make-instance 'exil-environment)))))

(defmacro setenv (name)
  (let ((env (gethash (symbol-name name) *environments*)))
    (when env (setf *current-environment* env))))

;; creates reader function <slot-name> and writer function set-<slot-name>
;; for the environment class, also creates setf macro
;; i used this instead of easier :accessor possibility, for this way
;; i could supply a default value for the environment parameter
(defmacro exil-env-accessor (slot-name)
  `(progn
     (defun ,slot-name (&optional (environment *current-environment*))
       (slot-value environment ',slot-name))
     (defsetf ,slot-name (&optional (environment *current-environment*)) (value)
       `(setf (slot-value ,environment ',',slot-name) ,value))))

(defmacro exil-env-accessors (&rest slot-names)
  `(progn ,@(loop for slot-name in slot-names
	       collect `(exil-env-accessor ,slot-name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (exil-env-accessors facts fact-groups templates rules rete))
;; rete should be removed after proper DEBUG

(defun add-fact (fact &optional (environment *current-environment*))
  (my-pushnew fact (facts environment) :test #'fact-equal-p)
  (add-wme fact))

(defun add-fact-group (group-name fact-descriptions
		       &optional (environment *current-environment*))
  (if (assoc group-name (fact-groups environment))
      (setf (assoc-value group-name (fact-groups environment))
	    fact-descriptions)
      (push (cons group-name fact-descriptions)
	    (fact-groups environment))))

(defun add-template (template &optional (environment *current-environment*))
  (setf (gethash (symbol-name (name template)) (templates environment)) template)
  template)

(defun find-template (name &optional (environment *current-environment*))
  (gethash (symbol-name name) (templates environment)))

(defun add-rule (rule &optional (environment *current-environment*))
  (setf (gethash (symbol-name (name rule)) (rules environment)) rule)
  (new-production rule (rete environment))
  rule)

(defun find-rule (name &optional (environment *current-environment*))
  (gethash (symbol-name name) (rules environment)))

(defun reset-environment (&optional (environment *current-environment*))
  (setf (facts environment) ()
	(fact-groups environment) ()
	(templates environment) (make-hash-table :test 'equalp)
	(rules environment) (make-hash-table :test 'equalp)
	(rete environment) (make-instance 'rete))
  nil)

;; DEPENDENCIES:
;; these functions had to be extracted here from theire original files
;; (templates.lisp, facts.lisp, patterns.lisp) because of therire
;; dependency on find-template function. this wouldn't be needed
;; if i knew about something similar to C's forward definitions

;; tmpl-object searches template's slot list, finds values from them in
;; specification or falls back to default values if he finds nothing
;; if there's some other crap in specification, tmpl-object doesn't care,
;; the only condition is, that (rest specification) has to be plist
(defun tmpl-object (specification object-type)
  (let ((template (find-template (first specification))))
    (cl:assert template () "can't find template ~A" (first specification))
    (make-instance
     object-type ;; >>>>>>>>>>>>>> cat's standing on my keyboard
     :tmpl-name (first specification)
     :slots (loop with initargs = (rest specification)
		 for slot-spec in (slots template)
		 collect (cons (car slot-spec)
			       (or (getf initargs
					 (to-keyword (car slot-spec)))
				   (getf (cdr slot-spec)
					 :default)
				   (class-slot-value object-type 'slot-default)))))))

(defun tmpl-object-specification-p (specification)
  (and (listp specification)
       (find-template (first specification))
       (or (null (rest specification))
	   ;; probably faster than (= (length specification) 1)
	   (keywordp (second specification)))))

;; tmpl-fact searches template's slot list, finds values from them in
;; fact-spec or falls back to default values if he finds nothing
;; if there's some other crap in fact-spec, tmpl-fact doesn't care,
;; the only condition is, that (rest fact-spec) has to be plist
(defun tmpl-fact (fact-spec)
  (tmpl-object fact-spec 'template-fact))

(defun tmpl-fact-specification-p (fact-spec)
  (tmpl-object-specification-p fact-spec))

(defun make-fact (fact-spec)
  (if (tmpl-fact-specification-p fact-spec)
      (tmpl-fact fact-spec)
      (make-instance 'simple-fact :fact fact-spec)))

(defun tmpl-pattern (pattern-spec)
  (tmpl-object pattern-spec 'template-pattern))

(defun tmpl-pattern-specification-p (specification)
  (tmpl-object-specification-p specification))

(defun make-pattern (specification)
  (if (tmpl-fact-specification-p specification)
      (tmpl-pattern specification)
      (make-instance 'simple-pattern :pattern specification)))
