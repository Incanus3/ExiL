;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/object.lisp - utilities for presenting objects
Copyright (c) 1998,1999 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

-------

This file is part of the SPRES package..

|#

(in-package :spres-impl)

(defvar *special-doc-handlers* (make-hash-table :test #'equal))
(defvar *special-info-handlers* (make-hash-table :test #'equal))

(defun establish-doc-handler& (key fun)
  "adds a function to *special-doc-handlers*.  Such a function 
should take three arguments.. the document, the list of doc-obj
to handle and a special argument (type T) with extra info (usually
a keyword, e.g :class)."
  (setf (gethash key *special-doc-handlers*) fun))

(defun get-doc-handler (key)
  "gets a function from *special-doc-handlers*"
  (gethash key *special-doc-handlers*))

(defun establish-info-handler& (key fun)
  "adds a function to *special-info-handlers*.  Such a function 
should take three arguments.. the document, the list of info-obj
to handle and a special argument (type T) with extra info (usually
a keyword, e.g :class)."
  (setf (gethash key *special-info-handlers*) fun))

(defun get-info-handler (key)
  "gets a function from *special-info-handlers*"
  (gethash key *special-info-handlers*))

;;; basic method to be invoked when all else fails
;;(defmethod-with-warn present-object (obj doc style))
(defmethod present-object (obj doc style) 
  (warn "Handler not written for (present-object ~a ~a ~a)" 
	(if (symbolp obj) (symbol-name obj) (its-name obj))
	(its-name doc) style)
  ;;  (when (eq obj t)
  ;;      (error "foo"))
  )

(defmethod get-method-arguments (meth-obj (prog-lang (eql :lisp)))
  "Returns a string with arguments organised properly, for common lisp."

  (let ((done-keydecl nil) ;; have we added the &key ?
	(done-optdecl nil) ;; have we added the &key ?
	(arglist (sdoc-method.args meth-obj))
	(fixed-args nil)
	(retargs '()))
    
    (flet ((fix-argument (x)
	     (let* ((info-lst (sdoc-arg.info x))
		    (name-lst (get-info-of-type info-lst "name"))
		    (type-lst (get-info-of-type info-lst "type"))
		    (mod-lst (get-info-of-type info-lst "mod"))
		    (defval-lst (get-info-of-type info-lst "defvalue"))
		    (name (if name-lst
			      (get-string sdoc-info.value (car name-lst))
			      ""))
		    (type (if type-lst
			      (get-string sdoc-info.value (car type-lst))
			    nil))
		    (defval (if defval-lst
				(get-string sdoc-info.value (car defval-lst))
				nil))
		    (mod (if mod-lst
			     (get-string sdoc-info.value (car mod-lst))
			     nil)))
	       
	     (when (and type (string-equal type "T"))
	       (setq type nil))
	     
	     (cond ((and (stringp mod)
			 (string-equal mod "keyword"))
		    (unless done-keydecl
		      (push "&KEY " retargs)
		      (setf done-keydecl t))
		    ;;(warn "kwd ~s" name)
		    (if (stringp defval)
			(strcat "(" name " " defval ")")
			name))
		   
		   ((and (stringp mod)
			 (string-equal mod "optional"))
		    (unless done-optdecl
		      (push "&OPTIONAL " retargs)
		      (setf done-optdecl t))
		    ;;(warn "kwd ~s" name)
		    (if (stringp defval)
			(strcat "(" name " " defval ")")
			name))

		   ((and (stringp mod)
			 (string-equal mod "rest"))
		    (push "&REST " retargs)
		    (if (stringp defval)
			(strcat "(" name " " defval ")")
			name))
		   
		   ((and (stringp mod)
			 (string-equal mod "body"))
		    (push "&BODY " retargs)
		    (if (stringp defval)
			(strcat "(" name " " defval ")")
			name))
		   
		   (type
		    (strcat "(" name " " type ")"))
		   (t
		    name))
	     )))

      (dolist (arg arglist)
	(push (fix-argument arg) retargs)
	(push " " retargs))

      ;;(warn "Got ~s" retargs)
      (setf retargs (apply #'concatenate 'string (nreverse (cdr retargs))))
      ;;(warn "Returning ~s" retargs)
      
      retargs)))



  
(defmethod get-method-arguments (meth-obj prog-lang)
  "Returns a string with arguments organised properly"

  (flet ((fix-argument (x)
	   (let* ((info-lst (sdoc-arg.info x))
		  (name-lst (get-info-of-type info-lst "name"))
		  (type-lst (get-info-of-type info-lst "type"))
		  (mod-lst (get-info-of-type info-lst "mod"))
		  (defval-lst (get-info-of-type info-lst "defvalue")))

	    
		    
	     (strcat
	      (if type-lst
		  (get-string sdoc-info.value (car type-lst))
		  "")
	      
	      (if name-lst
		  (strcat " " (get-string sdoc-info.value (car name-lst)))
		  "")
	      
	      (if defval-lst
		  (strcat " = " (get-string sdoc-info.value (car defval-lst)))
		  ""))
	     )))
  
    (let* ((arglist (sdoc-method.args meth-obj))
	   (fixed-args (mapcar #'fix-argument arglist)))
      
      (list-to-sep-string fixed-args :use-and-clause nil
			  :separator ","))
    ))



(defun get-method-retval (meth-obj doc)
  "Returns a string with the return value of the METH-OBJ."
  
  (flet ((fix-argument (x)
	   (let* ((info-lst (sdoc-retval.info x))
		  (type-lst (get-info-of-type info-lst "type"))
		  (defval-lst (get-info-of-type info-lst "defvalue")))

	     (cond ((is-prog-lang? :lisp)
		    (let ((act-type (if type-lst (get-string sdoc-info.value (car type-lst)) "T")))

		      act-type))

		   ;; not lisp
		   (t
		    (strcat
		     (if type-lst
			 (get-string sdoc-info.value (car type-lst))
			 "")
		     (if defval-lst
			 (strcat " = " (get-string sdoc-info.value (car defval-lst)))
			 "")
		     
		     ))))))
  
  (let* ((arglist (sdoc-method.retvals meth-obj))
	 (fixed-list (mapcar #'fix-argument arglist)))
    
    (list-to-sep-string fixed-list :and-word (get-word "and" doc)))))



(defmethod present-object ((obj sdoc-method) doc (style (eql :full)))
  (put doc "Full method: " (get-string sdoc-method.name obj) (get-newline doc)))

(defun put-together-meth-row (doc retval desc-word name arguments type-list
			      &key
			      visib
			      (style :traditional)
			      (linked t))
  "Pieces together a method-row from arguments."
  
  (case style
    (:traditional
     (strcat (if (and linked (not (has-spres-flag? :dont-print-anchor)))
		 (get-simple-anchor doc name)
		 "")

	     (if visib
		 (strcat visib " ")
	       "")
	     retval " " 
	     
	     (if (is-prog-lang? :lisp) 
		 ""
	       desc-word)
	     
	     (if (is-prog-lang? :lisp) " (" " ")
	     
	     (if linked
		 (get-linked-word doc name :method 
				  :desc (taggify doc :bold name))
		 (taggify doc :bold name))

	     (if (is-prog-lang? :lisp) " " " (")
	     
	     arguments
	     ")"
	     (if (< 0 (length type-list))
		 (strcat " [" type-list "]")
		 "")))
    (:intuitive
     (strcat (if (and linked (not (has-spres-flag? :dont-print-anchor)))
		 (get-simple-anchor doc name)
	       "")
	     
	     (if visib
		 (strcat visib " ")
	       "")
	     
	     (if (is-prog-lang? :lisp)
		 ""
	       desc-word)

	     (if (is-prog-lang? :lisp) " (" " ")

	     
	     (if linked
		 (get-linked-word doc name :method 
				  :desc (taggify doc :bold name))
		 (taggify doc :bold name))

	     (cond ((and (is-prog-lang? :lisp) (plusp (length arguments)))
		    " ")
		   ((is-prog-lang? :lisp) "")
		   (t " ("))
	     
	     arguments
	     
	     ")"
	     
	     (if (< 0 (length type-list))
		 (strcat " [" type-list "]")
		 "")
	     (if (> (length retval) 0) (strcat "  -->  " retval))
	     ""))
    (t
     "UNKNOWN METHOD STYLE WANTED")))

;;; this one can be memoized.. see tools/tests.lisp
(defun get-method-signature (doc obj &key (linked t) (style :traditional))
  "obj must be a sdoc-method"
  
  (let ((info-obj (sdoc-method.info obj))
	(any-types "")
	(visib (sdoc:get-visibility obj))
	(meth-word "function")
	(retvals ""))
    
    ;;      (warn "Method's info: ~a, and with type: ~a" info (get-info-of-type info "type"))
    
    (let ((type-list (get-info-of-type info-obj "mod")))
      (if type-list
	  (let ((presentable-type-list (mapcar #'caar (strip-info-fields type-list "type"))))
	    ;;	    (warn "pres: ~a" presentable-type-list)
	    
	    ;; this is pretty slow and dumbo.. fix later
	    (cond 
	      ((find "constructor" presentable-type-list :test #'string=)
	       (setq meth-word "constructor")
	       (setq presentable-type-list (remove "constructor" presentable-type-list :test #'string=))
	       ;; implicitly also a member
	       (setq presentable-type-list (remove "member" presentable-type-list :test #'string=)))
	       
	      ((find "destructor" presentable-type-list :test #'string=)
	       (setq meth-word "destructor")
	       (setq presentable-type-list (remove "destructor" presentable-type-list :test #'string=))
	       ;; implicitly also a member
	       (setq presentable-type-list (remove "member" presentable-type-list :test #'string=)))
	       
	      ((find "member" presentable-type-list :test #'string=)
	       (setq meth-word "method")
	       (setq retvals (get-method-retval obj doc))
	       (setq presentable-type-list (remove "member" presentable-type-list :test #'string=)))
	       
	      ((find "staticmember" presentable-type-list :test #'string=)
	       (setq meth-word "class-method")
	       (setq retvals (get-method-retval obj doc))
	       (setq presentable-type-list (remove "staticmember" presentable-type-list :test #'string=)))
	       
	      ;; we're a function.. or something with retval
	      (t (setq retvals (get-method-retval obj doc)) nil))
	      
	    (setq any-types (list-to-sep-string presentable-type-list :and-word (get-word "and" doc))))
	  
	  ;; if we have no type-list.. we're a function
	  (setq retvals (get-method-retval obj doc))))
      
      
    (put-together-meth-row doc
			   retvals
			   (get-word meth-word doc) 
			   (get-string sdoc-method.name obj) 
			   (get-method-arguments obj ?prog-lang)
			   any-types
			   :visib visib
			   :linked linked
			   :style style)
    ))


;; probably memoizable
(defun get-enum-values (enum-obj doc)
  "Returns a string with enum-values neatly organised"
  
  (let ((arglist (sdoc-enum.values enum-obj)))
    
    (list-to-sep-string (mapcar #'(lambda (x)
				    (strcat
				     (get-string sdoc-enumval.name x)
				     (if (sdoc-enumval.value x)
					 (strcat " = " (get-string sdoc-enumval.value x))
					 "")))
				
				arglist)
			:and-word (get-word "and" doc)
			)))

;;; this one can be memoized.. see tools/tests.lisp
(defun get-variable-signature (doc obj &key linked style)
  "obj must be a sdoc-variable"

  (declare (ignore linked style))
  
  (let ((any-types "")
	(info-obj (sdoc-variable.info obj))
	(var-word "variable")
	(visib (sdoc:get-visibility obj)))

    (when (and (is-prog-lang? :lisp) (typep ?parent 'sdoc-class))
      (setf var-word "slot"))
    ;;    (warn "Getting signature from ~a at ~a" (slot-value obj 'name) 
    ;;	  (slot-value obj 'location))
    
    (flet ((put-together-var-row (desc-word name expl the-type type-list &key visib)
	     (strcat (if visib (strcat (get-word visib doc) " ") "")
		     (get-word desc-word doc) " " (taggify doc :italic name) " " 
		     (get-word expl doc) " " (taggify doc :italic the-type)
		     (if (< 0 (length type-list))
			 (strcat " [" type-list "]")
			 ""))))


      
      (let ((type-list (get-info-of-type info-obj "mod")))
	(when type-list
	  (let ((presentable-type-list type-list))
	    ;;	    (warn "Checking ~a" presentable-type-list)
	    ;; why the a?
	    (dolist (a type-list)
	      (declare (ignore a))
	      (cond ((find "static" type-list 
			   :key #'(lambda (x) (car (sdoc-info.value x)))
			   :test #'string=) 
		     (setq var-word "class-variable")
		     (setq presentable-type-list (remove "static" presentable-type-list 
							 :key #'(lambda (x) (car (sdoc-info.value x)))
							 :test #'string=)))
		    ((find "constant" type-list 
			   :key #'(lambda (x) (car (sdoc-info.value x)))
			   :test #'string=) 
		     (setq var-word "constant")
		     (setq presentable-type-list (remove "constant" presentable-type-list 
							 :key #'(lambda (x) (car (sdoc-info.value x)))
							 :test #'string=)))
		    (t nil)))
	    ;;	    (warn "present ~a" presentable-type-list)
	    (when presentable-type-list
	      (setq any-types (list-to-sep-string
			       (mapcar #'(lambda (x) (get-string sdoc-info.value x))
				       presentable-type-list)
			       :and-word (get-word "and" doc)
			       ))))
	  
	  ))

      
      (let ((the-types (get-info-of-type info-obj "type"))
	    (its-type (if (is-prog-lang? :lisp)
			  "T"
			"&lt;unknown&gt;")))
	(when the-types
	  (setq the-types (mapcar #'caar (strip-info-fields the-types "type")))
	  (when the-types
	    (setq its-type (car the-types))))

	#||
	;; change
	(if (and (stringp any-types) (> (length any-types) 0))
	    (setq any-types "fix-me-type")
	    (setq any-types ""))
	||#
	;;	(warn "going ~a ~a ~a ~a" var-word (get-string sdoc-variable.name obj) its-type any-types)
	(put-together-var-row var-word
			      (get-string sdoc-variable.name obj)
			      "is of type"
			      its-type
			      any-types
			      :visib visib))
      )))


(defun get-class-signature (doc obj &key (linked t))
  "Returns a class-signature."

  (flet ((get-class-word (the-obj)
	   (cond ((and (is-prog-lang? :lisp) (tl-is-struct? the-obj))
		  "defstruct")
		 ((is-prog-lang? :lisp)
		  "defclass")
		 ((tl-is-interface? the-obj)
		  "interface")
		 (t
		  "class"))))
  
    (let* ((name (get-object-name obj))
	   (visib (sdoc:get-visibility obj))
	   ;;(is-interface (tl-is-interface? obj))
	   (class-word (get-class-word obj))
	   (inherits (get-inherit-obj obj)))

      (multiple-value-bind (interfaces other-inh)
	  (get-spec-inherits inherits "interface")
	
	(flet ((make-incl-str (inh-objs word)
		 (if inh-objs
		     (strcat (get-word word doc)
				       " "
				       (list-to-sep-string (if linked
							       (mapcar #'tl-make-link-for-class inh-objs)
							       (mapcar #'tl-make-string-desc inh-objs))
							   :and-word (get-word "and" doc))
				       " ")
		     "")))
	  

;;      (when (and inherits (is-prog-lang? :lisp))
;;	(warn "~a inherits.. ~s,~s" name inherits interfaces))

      (case ?prog-lang
	(:lisp
	 (strcat "(" class-word " " name " ("
		 (if other-inh
		     (list-to-sep-string (if linked
					     (mapcar #'tl-make-link-for-class other-inh)
					     (mapcar #'tl-make-string-desc other-inh))
					 :use-and-clause nil
					 :separator " ")
		     "")
		 ")" "(...))"))
	
	(otherwise
	 (strcat visib " " class-word " " name " "
		 (make-incl-str other-inh "extends")
		 (make-incl-str interfaces "implements") "{ ... };")))
      )))))

(defun get-class-facts (doc obj)
  "Returns a list of fun facts about a class."

  
  (let* (;;(name (get-object-name obj))
	 ;;(visib (sdoc:get-visibility obj))
	 (loc-list (slot-value obj 'location))
	 (parent-scope (etypecase ?parent
			 (sdoc-package (strcat "package " (make-obj-link doc ?parent nil
									 :desc (get-object-name ?parent))))
			 (sdoc-class (strcat "class " (make-obj-link doc ?parent nil
								     :desc (get-object-name ?parent))))
			 (nil nil)))
	 (inherits (get-inherit-obj obj))
;;	 (interfaces (get-spec-inherits inherits "interface"))
	 ;;(impl-string (if interfaces (strcat "implements " (list-to-sep-string interfaces) " ")
	;;		  ""))
	 (content-list (slot-value obj 'content))
	 (var-count (count-obj-types content-list 'sdoc-variable))
	 (meth-count (count-obj-types content-list 'sdoc-method))
	 (result nil)
	 )

    (multiple-value-bind (interfaces other-inh)
	(get-spec-inherits inherits "interface")
      
    
    (flet ((make-entry (name value)
	     (push (cons name value) result)))
    
     (when parent-scope
       (make-entry "scope" parent-scope))

      (when loc-list
	(let ((val (car (sdoc-location.file (car loc-list))))
	      (view-url (albert-setting '("albert" "docbook" "cvs-viewurl")))
	      (ver (setting-or-default '("albert" "docbook" "cvs-tag") "HEAD")))
	  
	  (when (plusp (length view-url))
	    (setf val (strcat "<ulink type=\"cvs\" url=\"" view-url val "?rev=" ver "\">"
			      (apispec-xml:xmlify-string val) "</ulink>")))
	
	  (make-entry "location" val)))

      (when other-inh
	(make-entry "inherits" (list-to-sep-string (mapcar #'tl-make-link-for-class other-inh)
						   :and-word (get-word "and" doc))))
      
      (when interfaces
	(make-entry "interfaces" (list-to-sep-string (mapcar #'tl-make-link-for-class interfaces)
						     :and-word (get-word "and" doc))))
      
      (when (and var-count (plusp var-count))
	(make-entry "# variables" var-count))
      
      (when (and meth-count (plusp meth-count))
	(make-entry "# methods" meth-count))

      (nreverse result)
      
      ))))

      

(defun get-spec-inherits (inh-list type)
  "Returns only specific inherits satisfying type."

  (let ((other nil)
	(satisfies-type nil))

    (dolist (i inh-list)
      (let ((satisfaction (let* ((info-list (get-info-of-type (slot-value i 'info) "how"))
				 (types (mapcar #'caar (strip-info-fields info-list "type"))))
			    (when (and types (consp types) (string-equal (car types) type))
			      i))))
	(if satisfaction
	    (push i satisfies-type)
	    (push i other))))

    (values satisfies-type other)))


(defun count-obj-types (list some-type)
  "Counts and returns number of types."
  (let ((counter 0))
    (dolist (i list)
      (cond ((typep i 'sdoc-category)
	     (let ((new-content (get-object-content i)))
	       (incf counter (count-obj-types new-content some-type))))
	    ((typep i some-type)
	     (incf counter))
	    (t nil)))
    counter))

(def-or-method is-empty? ((obj (or sdoc-class
				   sdoc-method
				   sdoc-variable
				   sdoc-enum
				   sdoc-typespec
				   sdoc-directive)))
;;  (declare (ignore obj))
  
  nil)

(defmethod is-empty? ((obj sdoc-package))
  (let ((content (slot-value obj 'content)))
;;    (warn "content is ~a" content)
    (if content
	nil
      t)))

(def-or-method is-empty? ((obj (or sdoc-module sdoc-category)))
  ;; should also check if any of the content is non-empty categories
  (let ((content (slot-value obj 'content)))
    (when content
      (dolist (x content)
	(unless (is-empty? x)
	  ;;	  (warn "~a wasn't empty" x)
	  (return-from is-empty? nil)))))

  ;;  (warn "~a was empty" (slot-value obj 'name))
  t)



(defgeneric handle-see-doc (document doc-objlist type)
  (:documentation "handles 'see' keyword"))
(defgeneric handle-param-doc (document doc-objlist type)
  (:documentation "handles 'param' keyword"))
(defgeneric handle-mod-info (document doc-objlist type)
  (:documentation "handles 'mod' keyword"))
(defgeneric handle-calls-info (document doc-objlist type)
  (:documentation "handles 'calls' keyword"))

(defun sym-has-prefix? (sym)
  (etypecase sym
    (string (let ((pos (position #\: sym)))
	      (when (integerp pos)
		t)))))

(defun split-sym-prefix (sym)
  (etypecase sym
    (string (let ((pos (position #\: sym)))
	      (when (integerp pos)
		(values (subseq sym 0 pos)
			(subseq sym (1+ (position #\: sym :from-end t))))
		)))))


(defun look-for-name-in-named-package (named-package name type)
  "Looks up a named-package and looks in it for NAME."

  (let ((pobj (look-for-name-in (get-sdoc-toplevel) named-package :package))
	(poss-obj nil))
    (cond ((consp pobj)
	   (setf poss-obj (look-for-name-in (car pobj) name type))
	   (when poss-obj
	     ;;(warn "Found ~s in ~s" poss-obj p)
	     (return-from look-for-name-in-named-package poss-obj)
	     ;;(cons (car poss-obj) (cdr poss-obj))) ;; this used to be wrong
	     ))
	  
	  (t
	   ;;(warn "Search in package ~s for ~s gave ~s" named-package name pobj)
	   nil))
    nil))


(defmethod look-for-name-in (obj name type)
  (unless-quiet
      (albert-warn "Fell through with ~s vs ~s vs ~s" obj name type))
  nil)

(defmethod look-for-name-in ((method sdoc-method) name type)
  nil)
#||
  (when (and (eq type :method)
	     (equal name (get-object-name method)))
    (cons method method)))
||#

(defmethod look-for-name-in ((method sdoc-toplevel) name (type (eql :method)))
  ;;(warn "top meth ~s" name)
  nil) ;; wrong place

(defmethod look-for-name-in ((top sdoc-toplevel) name (type (eql :package)))
  (dolist (i (slot-value top 'content))
    (when (and (typep i 'sdoc-package)
	       (equal name (get-object-name i)))
      (return-from look-for-name-in (cons i top))))
  nil) 

(defmethod look-for-name-in ((pack sdoc-class) name type)
;;  (when (string-equal name "SDOC-INHERIT.INFO")
;;    (warn "Foo ~s ~s" name type))
  
  (loop for i in (slot-value pack 'content)
	do
	(cond ((and (eq type :method) (typep i 'sdoc-method))
	       (when (equal (get-object-name i) name)
		 ;;(when (string-equal name "SDOC-INHERIT.INFO")
		   ;;(warn "Wee ~s in ~s" i pack)) 
		 (return-from look-for-name-in (cons i pack))))
	      ((and (eq type :class) (typep i 'sdoc-class))
	       (when (equal (get-object-name i) name)
		 (return-from look-for-name-in (cons i pack))))
	      ((typep i 'sdoc-variable) nil)
	      (t
	       (warn "Fell through in class ~s looking for ~s ~s" (get-object-name pack) name type)
	       nil)))
  nil)

(defmethod look-for-name-in ((pack sdoc-category) name type)
  (loop for i in (slot-value pack 'content)
	do
	(cond ((and (eq type :method) (typep i 'sdoc-method))
	       (when (equal (get-object-name i) name)
		 (return-from look-for-name-in (cons i pack))))
	      ((and (eq type :class) (typep i 'sdoc-class))
	       (when (equal (get-object-name i) name)
		 (return-from look-for-name-in (cons i pack))))
	      ((typep i 'sdoc-variable) nil)
	      (t
	       (warn "Fell through in category ~s looking for ~s ~s" (get-object-name pack) name type)
	       nil)))
  nil)


(defmethod look-for-name-in ((pack sdoc-package) name type)
  (loop for i in (slot-value pack 'content)
	do
	(cond ((and (eq type :method) (typep i 'sdoc-method))
	       (when (equal (get-object-name i) name)
		 (return-from look-for-name-in (cons i pack))))
	      ((typep i 'sdoc-method) nil)
	      ((and (eq type :class) (typep i 'sdoc-class))
	       (when (equal (get-object-name i) name)
		 (return-from look-for-name-in (cons i pack))))
	      ((typep i 'sdoc-class)
	       (let ((val (look-for-name-in i name type)))
		 (when val
		   ;;(when (string-equal name "SDOC-INHERIT.INFO")
		     ;;(break)
		     ;;(warn "back... ~s" val))
		   (return-from look-for-name-in val))))
	      ((typep i 'sdoc-variable) nil)
	      (t
	       (warn "fell through with ~s in pack ~s looking for ~s ~s" i (get-object-name pack) name type)
	       nil)))
  
  ;; now check packages it uses
  (let ((inherit-list (tl-get-fields "use" pack))
	(result nil))
    (dolist (p inherit-list)
      (setf result (look-for-name-in-named-package p name type))
      (when result
	(return-from look-for-name-in result)))
    nil))



(defmethod recursively-lookup-name ((from sdoc-toplevel) name (type (eql :method)))
  nil)

(defmethod recursively-lookup-name (from name type)

  ;; first we should do is to check _IN_ FROM
  #||
  (when (and (stringp name)
	     (equal #\W (schar name 0)))
    ;;(warn "RLookup ~s ~s from ~s with stack ~s" type name from *scope-stack*)
    )
  ||#

  (let ((found nil))

    ;; if we have a package-prefix, go straight to named package
    (when (sym-has-prefix? name)
      (multiple-value-bind (pr sm)
	  (split-sym-prefix name)
	(assert (and pr sm))
	(setf found (look-for-name-in-named-package pr sm type))))

    (when found
      (return-from recursively-lookup-name found))

    ;; common one
    (setf found (look-for-name-in from name type))

    (when found
      ;;(when (string-equal name "SDOC-INHERIT.INFO")
	;;(warn "Foot ~s ~s ~s" name type found))

      ;;(warn "1> Found ~s from ~s" name from)
      (return-from recursively-lookup-name found))

    ;; invariant
    (assert (not (eq (parent-of from) from)))
    (block loop-upwards
      (let ((cur-ptr (parent-of from)))
	(loop

	 (unless cur-ptr
	   (return-from loop-upwards nil))

	 ;; an invariant that should hold
	 (when (eq (parent-of cur-ptr) nil)
	   (assert (typep cur-ptr 'sdoc-toplevel)))
	 
	 (setf found (look-for-name-in cur-ptr name type))
 
	 (when found
	   ;;(when (string-equal name "SDOC-INHERIT.INFO")
	     ;;(warn "Foot2 ~s ~s ~s" name type found))
	   (return-from loop-upwards t))
	 (setf cur-ptr (parent-of cur-ptr))
	 )))
    #||
    (when (and (not found) (not (eq (first *scope-stack*) from)))
      (assert (typep (parent-of from) 'apispec-xml:xml-class))

      (when (not (eq (first *scope-stack*) (parent-of from)))
	(warn "Lookup for ~s in ~s instead of ~s vs ~s" name (first *scope-stack*) from (parent-of from)))
      (setf found (look-for-name-in (first *scope-stack*) name type)))

    (when found
      ;;(warn "2> Found ~s from ~s" name (car *scope-stack*))
      (return-from recursively-lookup-name found))

    
    (unless found
      (let ((*scope-stack* (cdr *scope-stack*)))
	(setf found (recursively-lookup-name (first *scope-stack*) name type))))
    ||#
    (when found
      ;;(when (string-equal name "SDOC-INHERIT.INFO")
	;;(warn "3> Found ~s -> ~s" name found))
      (return-from recursively-lookup-name found))
    
    found))

(defmethod %check-for-method-dispatch (where obj)
  "Just returns NIl as default."
  (declare (ignore where obj))
  nil)

(defmethod %check-for-method-dispatch ((where sdoc-method) obj)
  "Checks for anything dispathing on OBJ in WHERE."

  (let ((collected '()))
    
    (dolist (i (slot-value where 'content))
      (let ((val (%check-for-method-dispatch i obj)))
	(cond ((eq val nil) nil)
	      ((consp val) (dolist (i val) (push i collected)))
	      ((typep val 'sdoc-method) (push val collected))
	      (t
	       (warn "Got odd ~s" val)))))
	       

    (dolist (arg (sdoc-method.args where))
      (let* ((info-lst (slot-value arg 'info))
	     (type-lst (get-info-of-type info-lst "type"))
	     (type-key nil))
	(when type-lst
	  (setf type-key (car (sdoc-info.value (first type-lst)))))
	(when (and (stringp type-key)
		   (string-equal (get-object-name obj) type-key))
	  (push where collected))))
    
    collected))



(defmethod %check-for-method-dispatch ((where sdoc-class) obj)
  "Checks for anything dispathing on OBJ in WHERE."
  (let ((collected '())
	(found nil))
    (dolist (o (slot-value where 'content))
      (setf found (%check-for-method-dispatch o obj))
      (cond ((eq found nil) nil)
	    ((consp found)
	     (dolist (i found) (push i collected)))
	    (t
	     (warn "PF: ~s" found))))
    collected))

(defmethod %check-for-method-dispatch ((where sdoc-package) obj)
  "Checks for anything dispathing on OBJ in WHERE."
  (let ((collected '())
	(found nil))
    (dolist (o (slot-value where 'content))
      (setf found (%check-for-method-dispatch o obj))
      (cond ((eq found nil) nil)
	    ((consp found)
	     (dolist (i found) (push i collected)))
	    (t
	     (warn "PF: ~s" found))))
    collected))

(defmethod %check-for-method-dispatch ((where sdoc-toplevel) obj)
  "Checks for anything dispathing on OBJ in WHERE."
  (let ((collected '())
	(found nil))
    (dolist (o (slot-value where 'content))
      (setf found (%check-for-method-dispatch o obj))
      (cond ((eq found nil) nil)
	    ((consp found)
	     (dolist (i found) (push i collected)))
	    (t
	     (warn "TF: ~s" found))))
    collected))

  
(defun find-related-methods (obj)
  ;; let's do it simply, just iterate over everything and assume it's
  ;; what we ask for based on name, instead of checking via package-use and symbol-lookup

  (let ((toplevel nil)
	(collected '()))
    (dolist (i *scope-stack*)
      (when (typep i 'sdoc-toplevel) (setf toplevel i)))

    (setf collected (%check-for-method-dispatch toplevel obj))
    (if (listp collected)
	collected
	(list collected))))
  
(defun get-export-table (obj)
  "Returns an export-table for the given package object."
  (let ((table (make-hash-table :test #'equal))
	(export-list (tl-get-fields "export" obj)))
    (dolist (i export-list)
      (setf (gethash i table) t))
    table))

(defmethod is-exported? (obj name)
  "Returns T if the NAME is exported from the current/active package."
  (gethash name *package-exports*))

(defmethod is-exported? ((obj sdoc-method) name)
  "Returns T if the NAME or base-part of NAME is exported from the current/active package."
  (or (gethash name *package-exports*)
      (gethash (%strip-setf name) *package-exports*)))


(defmethod present-book-header (doc actual-stream)
  nil)

(defmethod present-book-footer (doc actual-stream)
  nil)

(defun present-book (tpl-object)
  "Outputs a book of the given top-level object"
  
  (when-verbose
      (albert-info "spres> attempting to make a book out of ~a" tpl-object))

  ;; maybe we should check for hyperspec?
  (when-bind (clhs-root (albert-setting '("hyperspec" "root")))
    (register-clhs-root clhs-root))
    
  (let ((?file-table (calculate-file-list tpl-object nil))
	(document (make-document ?outdir
				 "book"
				 ?format
				 ?language)))

    (tl-ensure-file-dirs ?file-table ?outdir)
	    
    ;;(cl-user::pr-ht ?file-table)
    
    (let ((*scope-stack* (cons tpl-object *scope-stack*)))
      (present-object tpl-object document :full))

    
    (present-document document :content-prefix #'present-book-header
		      :content-suffix #'present-book-footer)
    
    (ignore-errors
      (unless-quiet
       (albert-info "spres> wrote docbook book to ~a~a~a" (document.directory document)
		    (document.filename document)
		    (get-file-extension document))))


    ))
