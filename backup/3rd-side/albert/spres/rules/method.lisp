;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/rules/method.lisp - the rules for methods
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres-impl)


(def-rule-info
    ((:key :generic-fun-purpose)
     (:name print-purpose)
     (:req (?class sdoc-method)
	   (?document-type docbook-document))
     )

    (let ((purpose ""))

      (let ((fields (tl-get-fields "purpose" obj)))
	;; add if any found..
	(when fields (warn ">> Purpose ~a" fields)))

      #||
      (unless (plusp (length purpose))
	(let ((fields (tl-get-fields "documentation" obj)))
	  (when (and fields (stringp (first fields)))
	    (setf purpose (first fields)))))
      ||#

      (unless (plusp (length purpose))
	(cond ((is-generic-fun? obj)
	       (setf purpose (albert-setting '("albert" "presentation" "default-purpose-string" "generic"))))
	      (t
	       (setf purpose (albert-setting '("albert" "presentation" "default-purpose-string" "method"))))))

      (when (eq purpose nil)
	(setf purpose ""))

            
      (put doc "  <refpurpose>" purpose "</refpurpose>" (eol))

      ))

(def-rule-info
    ((:key :full-tpl-method/docbook)
     (:name present-object)
     (:desc "Presents a full GF.")
     (:req (?class sdoc-method)
	   (?document-type docbook-document)
	   (?context (eql :full))
;;	   (eq ?prog-lang :java)
	   ;; assertion, we should be below a class, ie a refsect1
	   (eq ?rec-state :reference)
	   )
     (:variation
      :never-print-execute))

    ;;(warn "Doing generic ~s for ~s" (get-object-name obj)
	;;  (mapcar #'get-object-name (sdoc-method.content obj))))

    (with-ok-obj-document (doc obj doc)
      (let ((cl-name (get-object-name obj))
	    (print-class-details t))

	(put doc "<refentry id=\"" (make-obj-id doc obj ?parent) "\">" (eol))

	(put doc "<refnamediv>" (eol)
	     "<refname>" cl-name "</refname>" (eol))
      
	(print-purpose obj doc context)

	(put doc "</refnamediv>" (eol))

	(when-bind (doc-list (sdoc-method.doc obj))
	  (dolist (i doc-list)
	    (let ((type (slot-value i 'type))
		  (text (slot-value i 'text)))
	      
	      (cond ((equal "desc" (first type))
		     (put doc
			  " <refsynopsisdiv>" (eol)
			  "  <title class=\"contenttitle\">" (get-word "Description" doc) "</title>" (eol)
			  "  <simpara>" (apispec-xml:xmlify-string (first text)) "</simpara>" (eol)
			  " </refsynopsisdiv>" (eol)))
		    (t
		     (warn "Albert: Unknown GF docs ~s ~s ~s" cl-name type text))))))
	
	(add-spres-flag! :in-gf)
	(let* ((?parent obj))
	  (present-with-content-manager obj doc (slot-value obj 'content)))

	(remove-spres-flag! :in-gf)
	
	(put doc "</refentry>" (eol))
	)))

    

(def-rule-info
    ((:key :full-method/docbook)
     (:name present-object)
     (:desc "Presents a full method.")
     (:req (?class sdoc-method)
	   (?document-type docbook-document)
	   (?context (eql :full))
;;	   (eq ?prog-lang :java)
	   ;; assertion, we should be below a class, ie a refsect1
	   (or (eq ?rec-state :refsect1)
	       (eq ?rec-state :refsect2))
	   )
     (:variation
      :never-print-execute))

    ;;(when (should-have-individual-file-p obj ?context)
      ;;(return-from present-object t))
    
  (let ((meth-name (get-object-name obj))
	(exportclass ""))
    
    (unless meth-name
      (setq meth-name "{not-named}"))

    (when (is-exported? obj meth-name)
      (setf exportclass " class=\"exported\""))
    ;;(warn "EXPORT for ~s is ~s" meth-name (is-exported? obj meth-name))

    #||
    (when (string-equal meth-name "SDOC-VARIABLE.LOCATION")
      (warn "S-V.LOC ~s vs ~s vs ~s ~s -> ~s" obj ?parent
	    (has-spres-flag? :related-methods) *scope-stack*
	    (make-obj-id doc obj ?parent)))
    ||#
    
    (put doc
	 "<formalpara" exportclass ">" (eol)
	 "<title>"
	 (if (or (has-spres-flag? :in-gf)
		 (has-spres-flag? :dont-print-anchor))
	     ""
	     (get-simple-anchor doc (make-obj-id doc obj ?parent :allow-cached nil)))
	 
	 (cond ((has-spres-flag? :related-methods)
		(let (;;(guess-package (find-if #'(lambda (x) (typep x 'sdoc-package)) *scope-stack*))
		      (up-package (get-enclosing-package obj)))
		  #||
		  (unless (string-equal (get-object-name guess-package) (get-object-name up-package))
		    (warn "Mismatch in enclosing package for obj ~s: ~s vs ~s"
			  obj (get-object-name guess-package) (get-object-name up-package)))
		  ||#

		  ;;(warn "For ~s we have ~s stack and ~s parent" meth-name  *scope-stack* ?parent)
		  (if up-package
		      (concatenate 'string (get-object-name up-package) ":")
		      "")))
	       (t
		""))
	 meth-name
	 "</title>"
	 (eol))
    
    (put doc "<para>" (eol))
    
    ;; improve this later.. 
    (let* ((the-obj obj)
	   (doc-list (sdoc-method.doc the-obj))
	   (loc-list nil)
	   (show-loc nil)
	   (not-wanted (list "mod" "class" "dispatch" "language" ))
	   (info-list nil))

      (when-bind (where (slot-value obj 'where))
	(setf loc-list (flatten (loop for x in where
				      collecting (slot-value x 'location)))))
      
      (when (and loc-list (tl-show-location? :variable)
			  (not (has-spres-flag? :dont-print-location)))
	(setf show-loc t))
      
      (unless (albert-setting '("albert" "presentation" "funcallable" "calls"))
	(push "calls" not-wanted))
      (unless (albert-setting '("albert" "presentation" "funcallable" "calledby"))
	(push "calledby" not-wanted))

      (setf info-list (get-info-except-types (sdoc-method.info obj) not-wanted))
      
      ;;(warn "reduced ~s to ~s" (sdoc-method.info obj) info-list)

      (put doc "<programlisting>"
	   (apispec-xml:xmlify-string (get-method-signature doc the-obj :linked nil :style :intuitive))
	   "</programlisting>" (eol))
      
      
      (when (or doc-list info-list)
	(put doc "<variablelist>" (eol))
	(db-present-doc-list  doc-list doc :method :suppress-wrapper t)
	(db-present-info-list info-list doc :method :suppress-wrapper t)
	(when show-loc
	  (setf loc-list (mapcar #'possibly-cvs-link loc-list))
	  (db-print-location loc-list doc :variable :suppress-wrapper t))
	(put doc "</variablelist>" (eol))
	))
      
    (put doc "</para></formalpara>" (eol)))

  ;; small-gf hack
  (when-bind (content (sdoc-method.content obj))
    (add-spres-flag! :in-gf)
    (dolist (i content)
      (present-object i doc ?context))
    (remove-spres-flag! :in-gf))
  
  )
