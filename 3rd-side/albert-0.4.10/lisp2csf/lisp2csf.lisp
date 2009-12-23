;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: LISP2CSF -*-

#|

DESC: lisp2csf/lisp2csf.lisp - reads lisp-code and generates CSF
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

-------

|#


(in-package :lisp2csf)

(defvar *top-objects-csf* nil)
(defvar *current-package* nil)
(defvar *current-file* nil)
(defvar *previous-structs* '())
(defvar *packages-to-wipe* nil)
(defvar *package-table* (make-hash-table :test #'equal))

(defun %get-cur-file ()
  (if *current-file* *current-file* "unknown.file"))

(defun find-lisp2csf-package-obj (pack-name)
  (let (;;(table *top-objects-csf*)
	(table *package-table*))
    (gethash pack-name table)))

(defun (setf find-lisp2csf-package-obj) (val pack-name)
  (let (;;(table *top-objects-csf*)
	(table *package-table*))
    ;;(warn "Adding ~s -> ~s" pack-name (get-object-name val))
    (setf (gethash pack-name table) val)))


(defgeneric analyse-object (objtype obj)
  (:documentation "analyses the given object"))

(defmethod analyse-object (objtype obj)
  ;; default, do nothing
  (declare (ignore obj objtype))
  ;;(warn ">> Fell through with ~s ~s" (package-name (symbol-package objtype)) objtype)
  t)

(defvar *special-forms* '(block catch eval-when flet
			  function go if labels
			  let let* load-time-value
			  locally macrolet multiple-value-call
			  multiple-value-prog1 progn
			  progv quote return-from
			  setq symbol-macrolet tagbody
			  the throw unwind-protect))

(defvar *ignorable-calls* (list 'dolist 'eq 'eql 'equal 'dotimes '+ '- '* '/
				#+cmu 'common-lisp::backq-append
				#+cmu 'common-lisp::backq-cons
				#+cmu 'common-lisp::backq-list
				#+cmu 'common-lisp::backq-list*
				#+sbcl 'sb-impl::backq-append
				#+sbcl 'sb-impl::backq-cons
				#+sbcl 'sb-impl::backq-list
				#+sbcl 'sb-impl::backq-list*
				))

(defun dummy-macro-dispatcher (stream a-char some-val)
  "hack."
  (declare (ignore stream a-char some-val))
  (values))

(defun is-call? (sym)
  "checks if the symbol is valid as a call"
  (dolist (i *special-forms*)
    (when (eq i sym) 
      (return-from is-call? nil)))
  (dolist (i *ignorable-calls*)
    (when (eq i sym)
      (return-from is-call? nil)))
  (symbolp sym))

(defvar *cur-analysed-method* nil)
(defvar *cur-collected-calls* nil)

(defgeneric check-body-expression (expr-type expression))

(defmethod check-body-expression (expr-type expression)
  nil)

(defun analyse-body-expression (expr)
  (when (or (symbolp expr) (atom expr))
    (return-from analyse-body-expression nil))

  (assert (consp expr))
  (case (first expr)
    (and (map nil #'analyse-body-expression (cdr expr)))
    (assert (unless (find (car expr) *ignorable-calls*)
	      (push (car expr) *cur-collected-calls*))
	    (analyse-body-expression (second expr)))
    #+allegro
    (excl::backquote nil)
    (block (map nil #'analyse-body-expression (cddr expr)))
    #+allegro
    (excl::bq-comma nil)
    #+allegro
    (bq-comma-atsign nil)
    (car (analyse-body-expression (cadr expr)))
    (cadr (analyse-body-expression (cadr expr)))
    (caddr (analyse-body-expression (cadr expr)))
    (cadddr (analyse-body-expression (cadr expr)))
    (case (analyse-body-expression (second expr))
      (loop for x in (cddr expr) 
	    do (map nil #'analyse-body-expression (cdr x))))
    (ccase (analyse-body-expression (second expr))
      (loop for x in (cddr expr) 
	    do (map nil #'analyse-body-expression (cdr x))))
    (cdr (analyse-body-expression (cadr expr)))
    (cddr (analyse-body-expression (cadr expr)))
    (cdddr (analyse-body-expression (cadr expr)))
    (check-type (unless (find (car expr) *ignorable-calls*)
		  (push (car expr) *cur-collected-calls*))
		(analyse-body-expression (second expr)))
    (cond (loop for x in (cdr expr)
		do (map nil #'analyse-body-expression x)))
    (ctypecase (analyse-body-expression (second expr))
      (loop for x in (cddr expr) 
	    do (map nil #'analyse-body-expression (cdr x))))
    (declare nil)
    (declaim nil)
    (define-condition nil)
    (defun (albert-info "lisp2csf> found defun ~s inside a body, ignored." (cadr expr)) nil)
    (destructuring-bind (map nil #'analyse-body-expression (cddr expr)))
    ;; add handling of DOs
    (do nil)
    (do* nil)
    ;; add for cdr
    (dolist (analyse-body-expression (second (second expr)))
      (map nil #'analyse-body-expression (cddr expr)))
    (do-symbols (push (car expr) *cur-collected-calls*) (map nil #'analyse-body-expression (cddr expr)))
    (do-external-symbols (push (car expr) *cur-collected-calls*) (map nil #'analyse-body-expression (cddr expr)))
    (do-all-symbols (push (car expr) *cur-collected-calls*) (map nil #'analyse-body-expression (cddr expr)))
    (dotimes (analyse-body-expression (second (second expr)))
      (map nil #'analyse-body-expression (cddr expr)))
    (ecase (analyse-body-expression (second expr))
      (loop for x in (cddr expr) 
	    do (map nil #'analyse-body-expression (cdr x))))
    (etypecase
	(when (is-call? (second expr))
	  (analyse-body-expression (second expr)))

      (loop for x in (cddr expr) 
	    do (when (consp x)
		 (map nil #'analyse-body-expression (cdr x)))))
    (eval-when (map nil #'analyse-body-expression (cddr expr)))
    ;; fix later
    (flet (dolist (i (second expr)) ;; check functions
	    ;; ignore name and args at this point
	    (map nil #'analyse-body-expression (cddr i)))
      (map nil #'analyse-body-expression (cddr expr)))
    (handler-case (analyse-body-expression (second expr))
      (dolist (i (cddr expr))
	(map nil #'analyse-body-expression (cddr i))))
    (if (map nil #'analyse-body-expression (cdr expr)))
    (labels (dolist (i (second expr)) ;; check functions
	      ;; ignore name and args at this point
	      (map nil #'analyse-body-expression (cddr i)))
      (map nil #'analyse-body-expression (cddr expr)))
    ;; fix cadr
    (lambda (map nil #'analyse-body-expression (cddr expr)))
    (let (dolist (i (second expr))
	   (when (consp i) (analyse-body-expression (second i))))
      (map nil #'analyse-body-expression (cddr expr)))
    (let* (dolist (i (second expr))
	    (when (consp i) (analyse-body-expression (second i))))
      (map nil #'analyse-body-expression (cddr expr)))
    (list (map nil #'analyse-body-expression (cdr expr)))
    (list*  (map nil #'analyse-body-expression (cdr expr)))
    ;; this one can be hell, so we try to fake things
    (loop (when (consp (second expr)) ;; do we have simple loop then?
	    (map nil #'analyse-body-expression (cdr expr)))
	  ;; we avoid complex loops
	  ;;(warn "Expanding ~s to ~s" expr (macroexpand-1 expr))
	  ;;(analyse-body-expression (macroexpand-1 expr))
	  nil) ;; complex
    (macrolet nil)
    ;; fix later
    (multiple-value-bind (map nil #'analyse-body-expression (cddr expr)))
    (multiple-value-setq (analyse-body-expression (third expr)))
    (not (map nil #'analyse-body-expression (cdr expr)))
    (or (map nil #'analyse-body-expression (cdr expr)))
    (proclaim nil)
    (progn (map nil #'analyse-body-expression (cdr expr)))
    (quote nil)
    (return-from (analyse-body-expression (caddr expr)))
    ;; evil hack!
    (set-dispatch-macro-character nil)
    (setf (loop for x on (cdr expr) by #'cddr
		do
		(progn
		  ;;(warn "Checking SETF on ~s" x)
		  (when (consp (first x))
		    ;;(warn "Call? ~s ~s" (first x) (caar x))
		    (push (list 'setf (caar x)) *cur-collected-calls*))
		  (when (is-call? (second x))
		    ;;(warn "After ~s comes ~s" (first x) (second x))
		    (analyse-body-expression (second x))))))
    (setq (map nil #'analyse-body-expression (cdr expr)))
    (the (map nil #'analyse-body-expression (cddr expr)))
    (typecase
	(when (is-call? (second expr))
	  (analyse-body-expression (second expr)))
      (loop for x in (cddr expr) 
	    do (map nil #'analyse-body-expression (cdr x))))
    (unless (map nil #'analyse-body-expression (cdr expr)))
    (values (map nil #'analyse-body-expression (cdr expr)))
    ;; (warn (map nil #'analyse-body-expression (cddr expr)))
    (when (map nil #'analyse-body-expression (cdr expr)))
		      
    (with-open-file (map nil #'analyse-body-expression (cddr expr)))
    (with-output-to-string (map nil #'analyse-body-expression (cddr expr)))
    (otherwise

     (cond ((and (symbolp (car expr)) (check-body-expression (car expr) expr))
	   ;; user handled this one
	    t)
	   ((not (apispec:proper-list? expr)) t) ;; skipped
	   ((is-call? (car expr))
	    (push (car expr) *cur-collected-calls*)
	    (map nil #'analyse-body-expression (cdr expr)))
	   (t
	    (map nil #'analyse-body-expression (cdr expr)))
	   ))
	       
    ))


(defmethod analyse-object ((objtype (eql 'defun)) obj)

  (let* ((fun-repr (cadr obj))
	 (fun-name (etypecase fun-repr
		     (symbol (string fun-repr))
		     (cons (format nil "~a" fun-repr))))
	 
	 (arg-list (caddr obj))
	 (body (cdddr obj)))
    
    (let ((meth-obj (%create-method fun-name
				    :type :function
				    :lambda-list arg-list
				    :body body)))
      (push meth-obj (slot-value *current-package* 'content))))

  t)

(defun lambda-flatten (l)
  "returns all non-conses in tree L in a single fresh list"
  (let ((before t))
    (cond ((null l) nil)
	  ((consp l)
	   (if (apispec:proper-list? l)
	       (loop for i in l nconcing
		     (cond ((eq i '&key) (setf before nil) (list i))
			   ((eq i '&optional) (setf before nil) (list i))
			   (before (lambda-flatten i))
			   (t (list i))
			   ))
	       (flatten l)))
	   (t (list l))
	  )))
		       
;; !!! must handle arguments differently from methods/functions
;;     no automatic match there.. different system needed?
(defmethod analyse-object ((objtype (eql 'defmacro)) obj)

  (let* ((fun-repr (cadr obj))
	 (fun-name (etypecase fun-repr
		     (symbol (string fun-repr))
		     (cons (format nil "~a" fun-repr))))
	(arg-list (caddr obj))
	(body (cdddr obj)))

    ;; bad hack, fix later
    (when (or (and (consp arg-list)
		   (not (apispec:proper-list? arg-list)))
	      (some #'consp arg-list))
      ;;(warn "GOT ~s" arg-list)
      ;; FIXME!
      ;; this isn't right, we should save the actual list
      ;; and then have a more intelligent "flatten"
      (setf arg-list (lambda-flatten arg-list)))
    
    (let ((meth-obj (%create-method fun-name
				    :type :macro
				    :lambda-list arg-list
				    :body body)))
      (push meth-obj (slot-value *current-package* 'content))))

  t)


(defmethod analyse-object ((objtype (eql 'defgeneric)) obj)

  (let* ((fun-repr (cadr obj))
	 (fun-name (etypecase fun-repr
		     (symbol (string fun-repr))
		     (cons (format nil "~a" fun-repr))))
	(arg-list (caddr obj))
	(options (cdddr obj)))
    
    (let ((meth-obj (%create-method fun-name
				    :type :generic
				    :lambda-list arg-list
				    :options options)))
      (push meth-obj (slot-value *current-package* 'content))))

  t)

(defmethod analyse-object ((objtype (eql 'defmethod)) obj)

  (multiple-value-bind (function-name qualifiers
				      lambda-list specializers body)
      (parse-defmethod (cdr obj))


    (let ((meth-obj (%create-method function-name
				    :type :method
				    :qual qualifiers
				    :lambda-list lambda-list
				    :spec specializers
				    :body body)))
;;      (warn "method ~s ~s ~s ~s" function-name qualifiers lambda-list specializers) 
      (push meth-obj (slot-value *current-package* 'content))))


  t)

;; evil hack
(defmethod analyse-object ((objtype (eql 'set-dispatch-macro-character)) obj)
  (albert-info "lisp2csf> Setting macro-dispatch on ~s~s to lisp2csf-dummy" (second obj) (third obj))
  (set-dispatch-macro-character (second obj) (third obj) #'dummy-macro-dispatcher))

(defun check-for-previous-struct (name)
  (dolist (i *previous-structs*)
    (when (string-equal name (car (csf-class.name i)))
      (return-from check-for-previous-struct i)))
  nil)
  

(defmethod analyse-object ((objtype (eql 'define-condition)) obj)
  ;; not implemented in albert yet, hopefully later
  t)

(defmethod analyse-object ((objtype (eql 'deftype)) obj)
  ;; not implemented in albert yet, hopefully later
  t)

(defmethod analyse-object ((objtype (eql 'define-compiler-macro)) obj)
  ;; not implemented in albert yet, hopefully later
  t)

(defmethod analyse-object ((objtype (eql 'define-method-combination)) obj)
  ;; not implemented in albert yet, hopefully later
  t)

(defmethod analyse-object ((objtype (eql 'define-modify-macro)) obj)
  ;; not implemented in albert yet, hopefully later
  t)

(defmethod analyse-object ((objtype (eql 'define-setf-expander)) obj)
  ;; not implemented in albert yet, hopefully later
  t)

(defmethod analyse-object ((objtype (eql 'define-symbol-macro)) obj)
  ;; not implemented in albert yet, hopefully later
  t)

(defmethod analyse-object ((objtype (eql 'defsetf)) obj)
  ;; not implemented in albert yet, hopefully later
  t)

(defmethod analyse-object ((objtype (eql 'proclaim)) obj)
  ;; not implemented in albert yet, hopefully later
  t)

(defmethod analyse-object ((objtype (eql 'declaim)) obj)
  ;; not implemented in albert yet, hopefully later
  t)

(defmethod analyse-object ((objtype (eql 'declare)) obj)
  ;; not implemented in albert yet, hopefully later
  ;; should it be on top-level?
  t)

(defmethod analyse-object ((objtype (eql 'in-package)) obj)

  (let ((pack-name (string-upcase (string (cadr obj)))))

    (multiple-value-bind (val found-p)
	(find-lisp2csf-package-obj pack-name)
      (unless found-p
	;;(albert-info "lisp2csf> found (in-package ~a), but have not seen it defined.  It's ok, I'll use a dummy."  pack-name)
	(setq val (%create-package-obj pack-name))
	(setf (find-lisp2csf-package-obj pack-name) val))

      ;; we set our package var to this new 
      (setq *current-package* val)))
  
  t)

(defmethod analyse-object ((objtype (eql 'defclass)) obj)

  (let ((cl-name (string (cadr obj)))
	(super-classes (caddr obj))
	(slot-info (cadddr obj))
	(options (cddddr obj)))

    (let ((the-class (%create-class (string-upcase cl-name)
				    :superclasses super-classes
				    :slots slot-info
				    :options options)))
      ;;(warn "Made ~a in ~a" the-class *current-package*)
      (push the-class (slot-value *current-package* 'content)))
  
    t))

(defmethod analyse-object ((objtype (eql 'defstruct)) obj)

  (let ((second-arg (second obj))
	(the-struct nil))

    ;;(warn "Checking defstruct ~s" second-arg)
    
    (cond ((symbolp second-arg)
	   (setf the-struct (%create-struct (string second-arg) :slots (cddr obj))))
	  ((consp second-arg)
	   (setf the-struct (%create-struct (string (first second-arg)) :slots (cddr obj)
					      :options (cdr second-arg))))
	  (t
	   (albert-warn "ODD STRUCT ~s" obj)))

    (when the-struct
      (push the-struct *previous-structs*)
      (push the-struct (slot-value *current-package* 'content)))

    t))


(defmethod analyse-object ((objtype (eql 'defvar)) obj)

  (let ((var-name (string (cadr obj))))
    ;;	(value (caddr obj)))

    (let ((the-var (%create-variable (string-upcase var-name))))
      ;;      (warn "Made ~a" the-class)
      (push the-var (slot-value *current-package* 'content)))
  
    t))

(defmethod analyse-object ((objtype (eql 'defparameter)) obj)

  (let ((var-name (string (cadr obj))))

    (let ((the-var (%create-variable (string-upcase var-name))))
      ;; mark it as a constant
      (push (fill-info-obj (make-csf-info) "mod" "parameter" nil) (slot-value the-var 'info))
      (push the-var (slot-value *current-package* 'content)))
  
    t))


(defmethod analyse-object ((objtype (eql 'defconstant)) obj)

  (let ((var-name (string (cadr obj))))
    ;;	(value (caddr obj)))

    (let ((the-var (%create-variable (string-upcase var-name))))
      ;; mark it as a constant
      (push (fill-info-obj (make-csf-info) "mod" "constant" nil) (slot-value the-var 'info))
      (push the-var (slot-value *current-package* 'content)))
  
    t))

(defmethod analyse-object ((objtype (eql 'export)) obj)
  (labels ((%export (word pack)
	     (cond ((consp word)
		    (dolist (i word)
		      (%export i pack)))
		   ((eq word 'quote) nil)
		   ((nonboolsym? word)
		    (push (fill-info-obj (make-csf-info)
					 "export"
					 (format nil "~a" word)
					 nil)
			  (csf-package.info pack)))
		   (t
		    (albert-warn "Export what?? ~s" word)))))
    
  (cond ((and (= (length obj) 2)
	      (consp (second obj)))
	 (dolist (e (second obj))
	   (%export e *current-package*)))

	((and (= (length obj) 3)
	      (consp (second obj)))
	 (cond ((string-equal (format nil "~a" (get-object-name *current-package*))
			      (format nil "~a" (third obj)))
		(dolist (e (second obj))
		  (%export e *current-package*)))
	       (t
		(unless-quiet
		    (albert-warn "Unable to find package ~s in ~a, ignoring."
				 (third obj) (list 'export "..." (third obj)))))
	       ))

	(t
	 (unless-quiet
	     (albert-warn "Does not handle export-forms like ~s yet." obj))))
  ))

(defmethod analyse-object ((objtype (eql 'eval-when)) obj)
  ;;  (warn "eval-when: ~s" obj)
  (dolist (i (cddr obj))
    ;;    (warn "eval-when going ~a" (car i))
    (analyse-object (car i) i)))

(defmethod analyse-object ((objtype (eql 'progn)) obj)
  ;;(warn "PROGN: ~s" obj)
  (dolist (i (cdr obj))
    ;;(warn "PROGN going ~a" (car i))
    (cond ((consp i)
	   (analyse-object (car i) i))
	  ((eq i nil) nil)
	  ((atom i) nil)
	  (t nil))))


(defmethod analyse-object ((objtype (eql 'let)) obj)
  ;;(warn "LET: ~s" obj)
  (dolist (i (cddr obj))
    ;;(warn "LET going ~a" (car i))
    (analyse-object (car i) i)))



(defmethod analyse-object ((objtype (eql 'defpackage)) obj)
  (when-verbose
      (albert-info "lisp2csf> found (defpackage ~s ...)" (cadr obj)))

  (let* ((the-name (string (cadr obj)))
	 (pack-name (string-upcase the-name))
	 (the-pack (find-lisp2csf-package-obj pack-name)))

    ;; if none was found we will create a new one
    (unless the-pack
      (setq the-pack (%create-package-obj (string-upcase the-name)))
      (setf (find-lisp2csf-package-obj pack-name) the-pack)
      (when-verbose
	  (albert-info "lisp2csf> new packagedef for ~a added" pack-name)))

    ;; time to collect the rest of the info
					      
    (let ((nicknames nil)
	  (exports nil)
	  (uses nil)
	  (shadow nil)
	  (shadowing-import nil)
	  (doc nil)
	  (r-l-package (find-package the-name)))
	
    
      (dolist (i (cddr obj))
	(when (consp i)
	  (case (car i)
	    (:use (setq uses (mapcar #'string (cdr i))))
	    (:shadow (setq shadow (mapcar #'string (cdr i))))
	    (:shadowing-import (setq shadowing-import (mapcar #'string (cdr i))))
	    (:export (setq exports (mapcar #'string (cdr i))))
	    (:nicknames (setq nicknames (mapcar #'string (cdr i))))
	    (:documentation (setq doc (string (cadr i))))
	    (t (unless-quiet
		(albert-warn "lisp2csf> no handler for package-option ~s [~a] yet" (car i) the-name)))
	    )))

      (%add-to-package the-pack :doc doc :uses uses :shadow shadow
		       :exports exports :nicknames nicknames
		       :shadowing-import shadowing-import)

      ;; register all nicknames
      (dolist (nick nicknames)
	(setf (find-lisp2csf-package-obj nick) the-pack))

      ;; add the package temporarily
      (unless r-l-package
	(let ((new-pack (make-package the-name :nicknames nicknames)))
	  (push new-pack *packages-to-wipe*)))
      
      )))
     

(defun %create-method (name &key type lambda-list qual spec body options docs)
  "Creates a csf-method which is returned, SPEC if a list of specialisers for methods."

  (declare (ignore qual))
  
  (let ((the-meth (make-csf-method)))
    
;;    (format t "creating method ~a ~s~%" name name)
    (setf (csf-method.name the-meth) (list (format nil "~a" name)))
    (setf (csf-method.id the-meth) (list (sds-global:%make-id+ "method"
							       :name name :fullname name
							       :scope  (%get-cur-file))))

    
    (let ((loc (make-csf-location))
	  (loc-wrap (make-csf-where)))
      (setf (csf-location.file loc) (list *current-file*))
      (setf (csf-where.what loc-wrap) (list "unknown"))
      (setf (csf-where.location loc-wrap) (list loc))
      (setf (csf-method.where the-meth) (list loc-wrap)))


    (push (fill-info-obj (make-csf-info)
			 "mod"
			 (string-downcase (string type))
			 nil)
	  (csf-method.info the-meth))

    ;; add docs
    (when (and body (stringp (car body)))
      (push (fill-info-obj (make-csf-info)
			 "documentation"
			 (car body)
			 nil)
	  (csf-method.info the-meth)))

    (dolist (i docs)
      (when (stringp i)
	(push (fill-info-obj (make-csf-info) "documentation" i nil)
	      (csf-method.info the-meth))))
    
    (dolist (i options)
      (when (consp i)
	(cond ((eq (car i) :documentation)
	       (unless (stringp (cadr i))
		 (warn "The documentation option of the ~a ~a does not have a string"
		       name type))
	       (push (fill-info-obj (make-csf-info)
			 "documentation"
			 (string (cadr i))
			 nil)
		     (csf-method.info the-meth)))
	      (t
	       (warn "Unknown method option ~a" i)))))

    ;; analyse body for calls
    (let ((*cur-analysed-method* the-meth)
	  (*cur-collected-calls* '()))
      (map nil #'analyse-body-expression body)
      (when *cur-collected-calls*
	(dolist (i (remove-duplicates *cur-collected-calls* :test #'eq))
	  (push (fill-info-obj (make-csf-info)
			       "calls"
			       (format nil "~s" i)
			       nil)
		(csf-method.info the-meth)))))
    
    
    (let* ((analysed-list (analyze-lambda-list lambda-list))
	   ;;(names (cadr analysed-list))
	   (collected-args nil))

      (destructuring-bind (&key required-names key-args optional-args rest-var body-var &allow-other-keys)
	  analysed-list

	#||
	(when (and (stringp name)
		 (equal #\S (schar name 0)))
	  (warn "~s has args ~s -> names ~s, keys ~s" name analysed-list required-names key-args))
	||#
	
      (loop for arg in required-names
	    for x from 0
	    do
	    (let ((some-arg (make-csf-arg))
		  (the-spec (if spec (nth x spec) nil)))
	      
	      (push (fill-info-obj (make-csf-info)
				   "name"
				   (format nil "~a" arg)
				   ;;(string arg)
				   nil)
		    (csf-arg.info some-arg))
	      
	      (when the-spec
		(push (fill-info-obj (make-csf-info)
				     "type"
				     (format nil "~a" the-spec)
				     nil)
		      (csf-arg.info some-arg)))
	      
	      (push some-arg collected-args)))

      (when optional-args
	(assert (consp optional-args))
	(loop for opt-arg in optional-args do
	      (cond ((nonboolsym? opt-arg) ;; single symbol, default is unspecified but nil
		     (let ((some-arg (make-csf-arg)))
		       (push (fill-info-obj (make-csf-info)
					    "name"
					    (format nil "~a" opt-arg)
					    nil)
			     (csf-arg.info some-arg))
		       (push (fill-info-obj (make-csf-info)
					    "mod"
					    "optional"
					    nil)
			     (csf-arg.info some-arg))
		       (push some-arg collected-args)))
		    
		    ((consp opt-arg) ;; arg with default value
		     (let ((some-arg (make-csf-arg)))
		       (push (fill-info-obj (make-csf-info)
					    "name"
					    (format nil "~a" (car opt-arg))
					    nil)
			     (csf-arg.info some-arg))
		       (push (fill-info-obj (make-csf-info)
					    "mod"
					    "optional"
					    nil)
			     (csf-arg.info some-arg))
		       (push (fill-info-obj (make-csf-info)
					    "defvalue"
					    (format nil "~s" (second opt-arg))
					    nil)
			     (csf-arg.info some-arg))
		       (push some-arg collected-args)))
		    
		    (t
		     (albert-warn "Unhandled optional argument ~s to function/method ~s"
				  opt-arg name))
		    )))

      
      (when key-args
	(assert (consp key-args))
	(loop for key-arg in key-args do
	      (cond ((nonboolsym? key-arg) ;; single symbol, default is unspecified but nil
		     (let ((some-arg (make-csf-arg)))
		       (push (fill-info-obj (make-csf-info)
					    "name"
					    (format nil "~a" key-arg)
					    nil)
			     (csf-arg.info some-arg))
		       (push (fill-info-obj (make-csf-info)
					    "mod"
					    "keyword"
					    nil)
			     (csf-arg.info some-arg))
		       (push some-arg collected-args)))
		    
		    ((consp key-arg) ;; arg with default value
		     (let ((some-arg (make-csf-arg)))
		       (push (fill-info-obj (make-csf-info)
					    "name"
					    (format nil "~a" (car key-arg))
					    nil)
			     (csf-arg.info some-arg))
		       (push (fill-info-obj (make-csf-info)
					    "mod"
					    "keyword"
					    nil)
			     (csf-arg.info some-arg))
		       (push (fill-info-obj (make-csf-info)
					    "defvalue"
					    (format nil "~s" (second key-arg))
					    nil)
			     (csf-arg.info some-arg))
		       (push some-arg collected-args)))
		    
		    (t
		     (albert-warn "Unhandled keyword-argument ~s to function/method ~s"
				  key-arg name))
		    )))

      (when rest-var
	(cond ((nonboolsym? rest-var) ;; single symbol, default is unspecified but nil
	       (let ((some-arg (make-csf-arg)))
		 (push (fill-info-obj (make-csf-info)
				      "name"
				      (format nil "~a" rest-var)
				      nil)
		       (csf-arg.info some-arg))
		 (push (fill-info-obj (make-csf-info)
				      "mod"
				      "rest"
				      nil)
		       (csf-arg.info some-arg))
		 (push some-arg collected-args)))
	      
	      (t
	       (albert-warn "Unhandled rest-var ~s to function/method ~s"
			    rest-var name))
	      ))

      (when body-var
	(cond ((nonboolsym? body-var) ;; single symbol, default is unspecified but nil
	       (let ((some-arg (make-csf-arg)))
		 (push (fill-info-obj (make-csf-info)
				      "name"
				      (format nil "~a" body-var)
				      nil)
		       (csf-arg.info some-arg))
		 (push (fill-info-obj (make-csf-info)
				      "mod"
				      "body"
				      nil)
		       (csf-arg.info some-arg))
		 (push some-arg collected-args)))
	      
	      (t
	       (albert-warn "Unhandled body-var ~s to function/method ~s"
			    body-var name))
	      ))
      
      
      (setf (csf-method.args the-meth) (nreverse collected-args))))

    
    the-meth))


(defun %create-variable (name &key initarg initform doc type)
  "creates a csf-variable which is returned."

  (declare (ignorable initarg initform))
  
  (let ((the-var (make-csf-variable)))
    (setf (csf-variable.name the-var) (list name))
    (setf (csf-variable.id the-var) (list (sds-global:%make-id+ "variable"
								:name name :fullname name
								:scope (%get-cur-file))))

    
    (let ((loc (make-csf-location)))
      (setf (csf-location.file loc) (list *current-file*))
      (setf (csf-variable.location the-var) (list loc)))

    (dolist (doc-str doc)
      (push (fill-info-obj (make-csf-info)
			   "documentation"
			   doc-str
			   nil)
	    (csf-variable.info the-var)))

    (when type
      (push (fill-info-obj (make-csf-info)
			   "type"
			   type
			   nil)
	    (csf-variable.info the-var)))
    
    the-var))

(defun %create-struct (name &key superclasses slots options)
  "creates a csf-class which is returned."
  
  (let ((the-class (make-csf-class))
	(function-prefix nil)
	(constructor-name nil)
	(predicate-name nil)
	(copier-name nil)
	(printer-name nil)
	(content nil)
	(info nil))
    
    (setf (csf-class.name the-class) (list name))
    (setf (csf-class.id the-class) (list (sds-global:%make-id+ "class"
							       :name name :fullname name
							       :scope  (%get-cur-file))))

    (setf function-prefix (strcat name "-")
	  constructor-name (strcat "MAKE-" name)
	  predicate-name (strcat name "-P")
	  copier-name (strcat "COPY-" name)
	  )
    
    (when (consp options)
      (dolist (i options)
	(cond ((and (consp i) (eq (first i) :conc-name))
	       (when (cdr i)
		 (let ((conc-name (second i)))
		   (if conc-name
		       (setf function-prefix (string conc-name))
		       (setf function-prefix nil)))))
	      ((and (consp i) (eq (first i) :constructor))
	       (when (cdr i)
		 (setf constructor-name (string (second i)))))
	      
	      ((and (consp i) (eq (first i) :predicate))
	       (when (cdr i)
		 (cond ((eq (second i) nil)
			(setf predicate-name nil))
		       (t
			(unless-quiet
			    (albert-info "lisp2csf> predicate-name ~s for struct ~s not handled yet." (second i) name))
			))))

	      ((and (consp i) (eq (first i) :include))
	       (when (cdr i)
		 (cond ((second i)
			(let ((parent (check-for-previous-struct (second i))))
			  (if parent
			      (dolist (i (csf-class.content parent))
				;; can be added right away
				(when (typep i 'csf-variable)
				  (push i content))
				;; we want some of these
				(when (typep i 'csf-method)
				  
				  ))
			      (albert-warn "lisp2csf> Unable to find parent-struct ~s for struct ~s."
				    (second i) name))
			  ))
		       (t
			(albert-warn "lisp2csf> :INCLUDE ~s for struct ~s not handled." (second i) name)))))

	      
	      ((and (consp i) (eq (first i) :copier))
	       (when (cdr i)
		 (cond ((eq (second i) nil)
			(setf copier-name nil))
		       (t
			(albert-warn "lisp2csf> copier-name ~s for struct ~s not handled." (second i) name)))))
	      
	      ((and (consp i) (eq (first i) :print-function))
	       (when (cdr i)
		 (cond ((nonboolsym? (second i))
			(setf printer-name (string (second i))))
		       ((eq (second i) nil)) ;; default, ignored
		       (t
			(albert-warn "lisp2csf> printer-name ~s for struct ~s not handled." (second i) name)))))
	      (t
	       (let ((opt-desc (if (consp i) (first i) i)))
		 (albert-warn "lisp2csf> albert has no handler yet for defstruct option ~s in ~s." opt-desc name)))
	      )))
    
    (let ((loc (make-csf-location)))
      (setf (csf-location.file loc) (list *current-file*))
      (setf (csf-class.location the-class) (list loc)))

    (dolist (i superclasses)
      (let ((inh-obj (make-csf-inherit)))
	(setf (csf-inherit.name inh-obj) (list (string i)))
	;; skip info now
	(push inh-obj (csf-class.parents the-class))))

    (push (fill-info-obj (make-csf-info) "mod" "struct" nil) info)

    (when (consp slots)
      (when (stringp (first slots))
	(push (fill-info-obj (make-csf-info) "documentation" (first slots) nil) info)
	(setf slots (cdr slots)))
      (dolist (i slots)
	(cond ((symbolp i)
	       (push (%create-variable (string-upcase (string i))
				       :type "T") content)
	       (let ((fun-name (if function-prefix
				   (strcat function-prefix (string-upcase (string i)))
				   (string-upcase (string i)))))
		 (push (%create-method fun-name :type :accessor
				       :spec (list name) :lambda-list '(obj)) content)
		 
		 (push (%create-method fun-name :type :function
				       :spec (list name) :lambda-list '(obj))
		       (csf-package.content *current-package*))
		 
		 (push (%create-method (strcat "(SETF " fun-name ")")
				       :type :function :spec (list name)
				       :lambda-list '(obj))
		       (csf-package.content *current-package*))))
	       
	       
	      ((consp i)
	       
	       (let ((slot-name (first i))
		     (initval (second i))
		     (type t)
		     (read-only nil)
		     (opts (cddr i)))
		 
		 (when opts
		   (loop for x on opts by #'cddr
			 do
			 (case (first x)
			   (:type (setf type (second x)))
			   (:read-only (cond ((eq (second x) t)
					      (setf read-only t))
					     ((eq (second x) nil)
					      (setf read-only nil))
					     (t
					      (albert-warn "lisp2csf> Unhandled value ~s for :read-only slot ~s in struct ~s"
						    (second x) slot-name name))))
			   (otherwise
			    (warn "SLOT-OPT for ~s in ~s is ~s"
				  slot-name name x)))))
		 
		 ;;(warn "slot ~s ~s" name initval)
		 (push (%create-variable (string-upcase (string slot-name))
					 :initform (string-upcase (format nil "~a" initval))
					 :type (format nil "~a" type) ;;(string-upcase (string type))
					 )
		       content)
		 
		 (let ((fun-name (strcat function-prefix (string-upcase (string slot-name)))))
		   ;;(warn "Accessor for ~s is ~s" name (strcat function-prefix (string-upcase (string name))))
		   (push (%create-method fun-name
					 :type (if read-only :reader :accessor)
					 :spec (list name) :lambda-list '(obj)) content)
		   
		   (push (%create-method fun-name
					 :type :function :spec (list name) :lambda-list '(obj))
			 (csf-package.content *current-package*))
		   
		   ;;(warn "adding ~s" fun-name)
		   (unless read-only
		     (push (%create-method (strcat "(SETF " fun-name ")")
					   :type :function :spec (list name) :lambda-list '(obj))
			   (csf-package.content *current-package*))))
		 
		 ))

	      (t
	       (warn "ODD SLOT ~s in ~s" i name))
	      )))

    (when constructor-name
      (push (%create-method constructor-name :type :constructor :spec nil :lambda-list nil)
	    content)
      (push (%create-method constructor-name :type :constructor :spec nil :lambda-list nil)
	    (csf-package.content *current-package*)))
    
    (when predicate-name
      (push (%create-method predicate-name :type :predicate :spec (list t) :lambda-list '(obj))
	    content)
      (push (%create-method predicate-name :type :predicate :spec (list t) :lambda-list '(obj))
	    (csf-package.content *current-package*)))

    (when copier-name
      (push (%create-method copier-name :type :function :spec (list name) :lambda-list '(obj))
	    content)
      (push (%create-method copier-name :type :function :spec (list name) :lambda-list '(obj))
	    (csf-package.content *current-package*)))

    ;; add info about printer function to the struct object!
    (when (stringp printer-name)
      (push (fill-info-obj (make-csf-info) "printername" printer-name nil) info))

    
    (setf (csf-class.info the-class) (nreverse info))
    (setf (csf-class.content the-class) (nreverse content))
    
    the-class))

(defun %create-class (name &key superclasses slots options)
  "creates a csf-class which is returned."
  
  (let ((the-class (make-csf-class))
	(content nil)
	(info nil))
    
    (setf (csf-class.name the-class) (list name))
    (setf (csf-class.id the-class) (list (sds-global:%make-id+ "class"
							       :name name :fullname name
							       :scope  (%get-cur-file))))
    
    
    (let ((loc (make-csf-location)))
      (setf (csf-location.file loc) (list *current-file*))
      (setf (csf-class.location the-class) (list loc)))

    (dolist (i superclasses)
      (let ((inh-obj (make-csf-inherit)))
	(setf (csf-inherit.name inh-obj) (list (string i)))
	;; skip info now
	(push inh-obj (csf-class.parents the-class))))

    (dolist (opt options)
      (case (car opt)
	(:default-initargs
	    (when-verbose
		(albert-info "lisp2csf> found :default-initargs in class ~s, ignored."
			name)))
	(:documentation (push (fill-info-obj (make-csf-info)
					     "documentation"
					     (cadr opt)
					     nil)
			      info))
	(t (albert-warn "lisp2csf> unknown class option ~s in class ~s"
		 (car opt) name))))

    
    (dolist (slot slots)
      (cond ((consp slot)
	     (let ((initarg nil)
		   (initform nil)
		   (accessor nil)
		   (writer nil)
		   (reader nil)
		   (name-argument (list name))
		   (argument '(obj))
		   (allocation :instance)
		   (doc nil)
		   (type t)
		   (skip nil))
	       (loop for j on (cdr slot) do
		     (if skip
			 (setq skip nil)
			 (progn
			   (case (car j)
			     (:accessor (push (cadr j) accessor))
			     (:reader (push (cadr j) reader))
			     (:writer (push (cadr j) writer))
			     (:initarg (push (cadr j) initarg))
			     (:initform (push (cadr j) initform))
			     (:type (setf type (cadr j)))
			     (:documentation (push (cadr j) doc))
			     (:allocation (when (eq (second j) :class)
					    (setf allocation :class)))
			     (t
			      (albert-warn "lisp2csf> Unknown slot option ~s for slot ~s in class ~s"
				    (car j) (car slot) name)
			      ))
			     
			   (setq skip t))))
	       (let ((var (%create-variable (string-upcase (string (car slot)))
				       :initarg initarg
				       :initform initform
				       :type (format nil "~a" type)
				       :doc doc)))
		 
		 (when (eq allocation :class)
		   (push (fill-info-obj (make-csf-info) "mod" "static" nil) (csf-variable.info var)))
		 
		 (push var content))
	       

	       (flet ((%loc-make-method (obj type)
			(%create-method obj :type type :spec name-argument :lambda-list argument :docs doc)))

		 (dolist (nm accessor)
		   ;;(warn "Accessor ~s" nm)
		   (when (albert-setting '("albert" "lisp2csf" "accessors" "class"))
		     (push (%loc-make-method nm :accessor) content)
		     (push (%loc-make-method (list 'setf nm) :accessor) content))
		   (when (albert-setting '("albert" "lisp2csf" "accessors" "package"))
		     (push (%loc-make-method (list 'setf nm) :method) (csf-package.content *current-package*))
		     (push (%loc-make-method nm :method) (csf-package.content *current-package*))))
		   
		   
		 (dolist (nm reader)
		   (when (albert-setting '("albert" "lisp2csf" "accessors" "class"))
		     (push (%loc-make-method nm :reader) content))
		   (when (albert-setting '("albert" "lisp2csf" "accessors" "package"))
		     (push (%loc-make-method nm :method) (csf-package.content *current-package*))))
		 
		 (dolist (nm writer)
		   (when (albert-setting '("albert" "lisp2csf" "accessors" "package"))
		     (push (%loc-make-method nm :writer) content))
		   (when (albert-setting '("albert" "lisp2csf" "accessors" "package"))
		     (push (%loc-make-method nm :method) (csf-package.content *current-package*))))
		 )))

	    ;; just a single slotname, nothing else
	    ((nonboolsym? slot)
	     (let ((var (%create-variable (string-upcase (format nil "~a" slot))
				       :type "T")))
		 (push var content)))
	    
	    ;; something else entirely
	    (t
	     (albert-warn "lisp2csf> hoped for a slot in class ~s, got something else '~s'"
		   name slot)
	     nil)))

    (setf (csf-class.info the-class) (nreverse info))
    (setf (csf-class.content the-class) (nreverse content))

    ;;(warn "Returning class ~s in package ~s" the-class *current-package*)
    
    the-class))

(defun %add-to-package (package &key nicknames exports uses shadow shadowing-import doc)
  "Tries to do a modification of given package."
  
  (labels ((add-var-as-info (pack var word)
	     (unless (and var (stringp var))
	       (warn "ADD-VAR-AS-INFO got var-arg ~s" var)
	       (return-from add-var-as-info nil))
	     
	     (push (fill-info-obj (make-csf-info)
				  word
				  var
				  nil)
		   (csf-package.info pack)))
	   
	   (treat-var (pack var word)
	     (cond ((consp var)
		    (dolist (i var)
		      (add-var-as-info pack i word)))
		   ((stringp var)
		    (add-var-as-info pack var word))
		   ((eq var nil)
		    ;; do nothing
		    )
		   (t
		    (warn "Weird argument to treat-var: ~s" var)))) 
	   
	   (cleanse-for-duplicates (pack)
	     (setf (csf-package.info pack)
		   (remove-duplicates (csf-package.info pack)
				      :test #'equal-to)))
	   )

    (treat-var package nicknames "nickname")
    (treat-var package exports "export")
    (treat-var package uses "use")
    (treat-var package shadow "shadow")
    (treat-var package shadowing-import "shadowing-import")
    (treat-var package doc "documentation")
    
    (cleanse-for-duplicates package)

  
  ))
  
(defun %create-package-obj (name)
  "Creates and returns a package-obj"
  (let ((pack (make-csf-package))
	(where (%get-cur-file))
	(loc (make-csf-location)))
	  
    (setf (csf-package.name pack) (list name))
    (setf (csf-package.id pack) (list (sds-global:%make-id+ "package"
							    :name name :fullname name
							    :scope where)))

    ;;(when-verbose
    ;;(albert-info "lisp2csf> fixing location-object for ~a" name))
    (setf (csf-location.file loc) (list where))
    (setf (csf-package.location pack) (list loc))
 
    pack))

#||
(defun %create-package (name &key nicknames exports uses shadow shadowing-import doc)
  "creates a csf-package which is returned."
  
  (let ((pack (make-csf-package)))
    (setf (csf-package.name pack) (list name))
    (setf (csf-package.id pack) (list (sds-global:%make-id+ "package"
							    :name name :fullname name
							    :scope (%get-cur-file))))

    (flet ((add-var-as-info (var word)
	     (when var
	       (push (fill-info-obj (make-csf-info)
				    word
				    (list-to-sep-string var :use-and-clause nil)
				    nil)
		     (csf-package.info pack)))))
      
      (add-var-as-info exports "export")
      (add-var-as-info nicknames "nickname")
      (add-var-as-info shadow "shadow")
      (add-var-as-info shadowing-import "shadowing-import")
      (add-var-as-info uses "use"))

    (when doc
      (push (fill-info-obj (make-csf-info)
			   "documentation"
			   doc
			   nil)
	    (csf-package.info pack)))
    
    (let ((loc (make-csf-location)))
      (when-verbose
       (warn "fixing loc for ~a" name))
      (setf (csf-location.file loc) (list (%get-cur-file)))
      (setf (csf-package.location pack) (list loc)))
    
    ;;    (warn "creating package ~a ~a" pack (csf-package.info pack))
    
    pack))
||#

(defun update-pnames (list)
  (flet ((split-name (name)
	   (let ((pack (subseq name 0 (position #\: name)))
		 (sym (subseq name (1+ (position #\: name :from-end t)))))
	     (cons pack sym))))
    
    (let ((table (make-hash-table :test #'equal)))
      (dolist (i list)
	(let ((splitted (split-name i)))
	  (pushnew (cdr splitted) (gethash (car splitted) table) :test #'equal)))
      table)))

(defun update-packages (pack-list fname)
  "updates packages with list of package-symbols"
  (declare (ignore fname))
  
  (flet ((reader-casing (word)
	   #+allegro-v6.0 word
	   #-allegro-v6.0 (string-upcase word)))
  
  (let ((pack-table (update-pnames pack-list)))
    
    (when pack-table
      (maphash #'(lambda (k v)
		   ;;(warn "for file ~a I make package ~a [~a]" fname k v)
		   (let* ((pack-name (reader-casing k)) ;; fix later
			  (poss-package (find-package pack-name))
			  (sym))
		     (unless poss-package
		       (push (make-package pack-name) *packages-to-wipe*))
		     
		     (dolist (i v)
		       (let ((sym-name (reader-casing i))) ;; fix later
			 
			 (multiple-value-bind (found-p how)
			     (find-symbol sym-name pack-name)
			   (ignore-errors
			     ;;(warn "~a::~a is ~a,~a" pack-name sym-name found-p how)
			     (if found-p
				 (setq sym found-p)
				 (setq sym (intern sym-name pack-name)))
			     
			     (when (or (not poss-package) (eq how nil) (eq how :internal))
			       ;;(warn "exporting ~a" sym-name)
			       (export sym pack-name))))))))
	       pack-table)))))

  

(defun analyse-file (fname &key preprocess-packages called-recursively (use-clean-readtable t)
		     (ignore-missing-files t) (display-progress nil))
  "analyses one file and generates necessary csf-data"
  
  (when (or display-progress apispec-base:*verbose-operation*)
      (albert-info "lisp2csf> analysing ~s" fname)
    ;;(albert-info "lisp2csf>   pathname: ~s" (merge-pathnames (pathname fname)))
    )

  (flet ((do-reading ()
	   (let ((eof-sym (gensym))
		 #+cmu18e
		 (EXTENSIONS:*IGNORE-EXTRA-CLOSE-PARENTHESES* nil)
		 (the-fname (merge-pathnames (pathname fname))))
	     (unless (probe-file the-fname)
	       (cond (ignore-missing-files
		      (albert-warn "lisp2csf> didn't find the file: ~a" fname)
		      (return-from analyse-file nil))
		     (t
		      (error "Didn't find the file ~s" fname))))
	     (with-open-file (stream the-fname :direction :input)
	       (loop
		(handler-case 
		    (let* ((read-obj (read stream nil eof-sym)))
		      ;;(warn "read ~a" read-obj)
		     
		      (cond ((eq read-obj eof-sym)
			     (return-from do-reading t))
			    ((or (symbolp read-obj)
				 (stringp read-obj)
				 (numberp read-obj))
			     ;; ignore
			     )
			    ((consp read-obj)
			     (analyse-object (car read-obj) read-obj))
			    (t
			     (albert-warn "lisp2csf> read unexpected data ~s" read-obj))
			    ))

		  (reader-error (co)
		    (declare (ignore co))
		    ;;(warn "A reader-error occured: ~a" co)
		    )
		  #||
		  (error (co)
		    (warn "Error while reading ~s" co)
		    (describe co)
		    )
		  ||#
		  ))))))
  
    (let ((*current-file* (etypecase fname
			    (string fname)
			    (pathname (namestring fname))))
	  (*read-eval* nil))

      (unless (probe-file fname)
	(cond (ignore-missing-files
	       (albert-warn "lisp2csf> didn't find the file: ~a" fname)
	       (return-from analyse-file nil))
	      (t
	       (error "Didn't find the file ~s" fname))))
      
    (cond (called-recursively
	   (when preprocess-packages
	     (update-packages (figure-out-lisp-packages fname) fname))

	   (do-reading))

	  (t
	   (let ((*readtable* (if use-clean-readtable
				  (copy-readtable nil)
				  (copy-readtable)))
		 (*packages-to-wipe* nil))
	     (when preprocess-packages
	       (update-packages (figure-out-lisp-packages fname) fname))
	     
	     (do-reading)
	   
	     (dolist (i *packages-to-wipe*)
	       (albert-info "lisp2csf> Wiping[2] package ~a" (package-name i)) 
	       (delete-package i)))
	   

	   )))

  (values)))


(defun analyse-files (file-list &key out-file (preprocess-packages t) (ignore-missing-files t)
		      (use-clean-readtable t) (display-progress nil))
  "Analyses a list of files"
  
  (let ((*top-objects-csf* (make-hash-table :test #'equal))
	(*features* (cons :lisp2csf *features*))
	(*readtable* (if use-clean-readtable
			 (copy-readtable nil)
			 (copy-readtable)))
	(*previous-structs* '())
	(*packages-to-wipe* nil))
    

    (set-dispatch-macro-character #\# #\. #'dummy-macro-dispatcher)

	
#||
    ;; let's add the classic user package
    (let ((user-package (%create-package "USER")))
      (setf (gethash "USER" *top-objects-csf*) user-package))

    ;; some lisps crash when READing code with unknown packages
    (dolist (i '("FF"))
      (let ((pack (find-package i)))
	(unless pack
	  (push (make-package i) packages-to-wipe))))
||#

    (dolist (i file-list)
;;      (warn "going ~s" i)
      (analyse-file i :preprocess-packages preprocess-packages
		    :called-recursively t
		    :use-clean-readtable use-clean-readtable
		    :display-progress display-progress
		    :ignore-missing-files ignore-missing-files))
    

    ;; remove temporary packages
    (dolist (i *packages-to-wipe*)
      ;;(warn "Deleting package ~a" (package-name i))
      (delete-package i))


    (let ((objs-as-list (loop for x being the hash-values in *top-objects-csf*
			      collecting x))
	  (unique-packages (remove-duplicates
			    (loop for x being the hash-values in *package-table*
				  collecting x)))
	  (toplevel (make-csf-toplevel)))

      (setf objs-as-list (append unique-packages objs-as-list))
      
      (setf (csf-toplevel.language toplevel) (list "lisp"))
      (setf (csf-toplevel.content toplevel) objs-as-list)

      ;;(warn "Going verify..")

      (dolist (i objs-as-list)
	(verify-object i t))

      ;;(warn "Going to print tree [~a] to file.." (mapcar #'get-object-name objs-as-list))
      
      (when out-file
	(with-open-file (stream (merge-pathnames (pathname out-file))
				:direction :output
				:if-exists :supersede)
	  
	  (let ((csf-tool (apispec-xml:make-xml-tool (make-csf-factory))))
	    
	    (format stream "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>~%")
	    (format stream "<!DOCTYPE csf SYSTEM \"csf.dtd\">~%")
	    
	    (apispec-xml:print-as-xml toplevel stream csf-tool))))
      
      ;;(warn "xml-file printed..")
      
      (list toplevel))))


(defun retest (str)
  (with-input-from-string (is str)
    (read is)))
