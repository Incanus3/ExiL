;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/rules/package.lisp - the rules for packages
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres-impl)


(def-rule-info
    ((:key :package-header/docbook)
     (:name print-header)
     (:desc "Prints a package header for docbook.")
     (:req (?document-type docbook-document)
	   (?context (eql :package))
	   ))

    (let* (
	   (cl-name (get-object-name obj))
	   (title (strcat cl-name " package"))
	   (purpose "Exports/uses/nicknames/etc")
	   (inherit-list (tl-get-fields "use" obj))
	   (export-list (nreverse (tl-get-fields "export" obj)))
	   (desc (let ((fields (tl-get-fields "documentation" obj)))
		   (if fields (car fields) nil)))
	   (tname "package")
	   (tupname "Package")
	   (no-desc nil)

	  )

      ;;(warn "###doing package-header ~s ~s" cl-name ?parent)
  
      (put doc "<reference id=\"" (make-obj-id doc obj ?parent) "\">" (eol))
      
      (put doc "  <title>" tupname " " cl-name "</title>" (eol))
      
      (put doc " <refentry id=\"package" +id-word-delim+ "content" +id-word-delim+
	   (make-valid-entity cl-name) "\">" (eol))

      (tree-put doc `(:refmeta nil
		      (:refentrytitle nil ,title)
		      (:manvolnum nil 3)
		      (:refmiscinfo nil "Project name")))

      (tree-put doc `(:refnamediv nil
		      (:refname nil ,title)
		      (:refpurpose nil ,(if purpose purpose ""))
		      ))

      (unless no-desc
	(tree-put doc (list :refsect1 nil
			    (list :title nil "Description")
			    (list :para nil (if desc desc "[No description in DEFPACKAGE]"))
			    )))

      (when export-list
	(put doc "<refsect1>" (eol))
	(put doc " <title>" (get-word "Exports" doc) "</title>" (eol))
	(setf export-list (sort export-list #'string<))
	(cond ((> (length export-list) 3)
	       (db-print-table doc export-list :columns 2))
	      (t
	       (tree-put doc `(:simpara nil
			       ,(list-to-sep-string export-list :and-word (get-word "and" doc))))))
	(put doc "</refsect1>" (eol)))

      ;;(warn "foo")
      (when inherit-list
	;;(warn "inherit ~s" inherit-list)
	(setf inherit-list (mapcar #'(lambda (x)
				       (let ((lookup (recursively-lookup-name (get-sdoc-toplevel) x :package)))
					 (cond ((consp lookup)
						(make-obj-link doc (car lookup) nil ;;(cdr lookup)
							       :desc x))
					       (t
						;;(albert-warn "Failed to find package ~s used by ~s" x obj)
						(apispec-xml:xmlify-string x)))))
				   inherit-list))
	;;(warn "inherit ~s" inherit-list)
	(put doc "<refsect1>" (eol))
	(put doc " <title>" (get-word "Uses" doc) "</title>" (eol))
	(cond ((> (length inherit-list) 6)
	       (db-print-table doc inherit-list :columns 2 :xmlify nil))
	      (t
	       (tree-put doc `(:simpara nil
			       ,(list-to-sep-string inherit-list :and-word (get-word "and" doc))))))
	(put doc "</refsect1>" (eol)))

      
      (unless (has-spres-flag? :simple-package)
	(put doc "</refentry>" (eol)))
      ))

(def-rule-info
    ((:key :full-module/docbook)
     (:name present-object)
     (:desc "Presents a module.")
     (:req (?class sdoc-module)
	   (?document-type docbook-document)
	   (?context (eql :full))
	   ;;(eq ?prog-lang :java)
	   ))

    (print-header obj doc :package)

  (let ((cl-name (get-object-name obj))
	(content-list (slot-value obj 'content)))
  
    ;;       (put doc "<refentry id=\"package" +id-word-delim+
    ;;	    "entry" +id-word-delim+ cl-name "\">" (eol))
    
    (when content-list
      (let ((?rec-state :reference)
	    (?parent obj))
	(dolist (i content-list)
	  (present-object i doc context))))

    (put doc "</reference>" (eol))
    
    ))

(def-rule-info
    ((:key :full-generic-package/docbook)
     (:name present-object)
     (:desc "Presents lisp-specifics for a package.")
     (:req (?class sdoc-package)
	   (?document-type docbook-document)
	   (?context (eql :full))
;;	   (eq ?prog-lang :lisp)
	   ))

    (let ((*enclosing-package* obj)
	  (*package-exports* (get-export-table obj))
	  (*scope-stack* (cons obj *scope-stack*)))
      (multiple-value-bind (normal-objs individual-files)
	  (filter-away (slot-value obj 'content)
		       #'(lambda (x)
			   (should-have-individual-file-p x nil)))

	;; hackish filter
	(when (albert-setting '("albert" "presentation" "only-exported"))
	  ;; clean normal-objs for non-exported stuff
	  ;; maybe also do individual files here, but has tricky cases
	  ;;(warn "FILTER!")
	  (setf normal-objs (filter #'(lambda (obj)
					(when (is-exported? obj (get-object-name obj))
					  obj))
				    normal-objs)))

	
	(let* ((cl-name (get-object-name obj))
	       (?list-style :clever-sort)
	       ;;(?list-style :lisp-pack-sort)
	       (var-count (loop for x in normal-objs when (typep x 'sdoc-variable) summing 1))
	       (other-count (- (length normal-objs) var-count))
	       (simple-package t) ;; by default we like simple packages
	       )

	  (when individual-files
	    (setf simple-package nil))

	  (when (>= var-count (albert-setting '("albert" "presentation" "variables" "separatepage")))
	    (setf simple-package nil))

	  (when simple-package
	    (add-spres-flag! :simple-package))

	  ;; old shit
	  ;;(when (and (eq individual-files nil) normal-objs)
	  ;;  (add-spres-flag! :simple-package))
      
	  ;;(warn "doing package ~s" cl-name)
	  (print-header obj doc :package)
	  
	  
	  (let ((?rec-state :reference)
		(?parent obj))

	    
	    ;; we split methods from the other stuff
	    (let ((meths '())
		  (others '()))
	      (dolist (i individual-files)
		(if (typep i 'sdoc-method)
		    (push i meths)
		    (push i others)))
	      
	      ;; we do generics (later)
	      
	      ;; ok, now let's dump any other stuff this class had hidden
	      #||
	      (when others
	      (present-on-a-new-page (tl-sort-by-name others) doc context :title "Classes"
	      :filename "classlist"))
	      
	      (when meths
	      (present-on-a-new-page (tl-sort-by-name meths) doc context :title "Generic Functions"
	      :filename "genfunlist"))
	      ||#
	      (dolist (i (tl-sort-by-name others))
		(present-object i doc context))
	      
	      (dolist (i (tl-sort-by-name meths))
		(present-object i doc context))
	      
	      ))

	  
	  ;;      (warn "content ~a" (mapcar #'get-object-name content-list))
	  
	  (when normal-objs
	    (let* ((cl-name (get-object-name obj))
		   (file-id (strcat "package" +id-word-delim+ "contentlist" +id-word-delim+
				    (make-valid-entity cl-name)))
		   (fname (strcat (make-valid-entity cl-name) "/" "contentlist"))
		   (?rec-state :refsect1)
		   (?parent obj)
		   ;;(var-count (loop for x in normal-objs when (typep x 'sdoc-variable) summing 1))
		   (obj-count (length normal-objs))
		   (var-objs '())
		   ;;(other-count (- obj-count var-count))
		   )

	      
	      (register-separate-document (make-file-info :id file-id
							  :dir nil
							  :fname fname))
	      (with-ok-document (doc fname doc)

	      
	      (when (>= var-count (albert-setting '("albert" "presentation" "variables" "separatepage")))
		;; we wish to grab the variables now
		(let ((vars '())
		      (others '()))
		  (loop for i in normal-objs do
			(cond ((typep i 'sdoc-variable)
			       (push i vars))
			      ((typep i 'sdoc-method)
			       (push i others))
			      (t
			       (when-verbose
				   (albert-info "Non-method/var content ~s in package ~s" i cl-name))
			       (push i others))))
		  (setf var-objs (reverse vars)
			normal-objs (reverse others))))

	      ;;(warn "In package ~s there are ~s vars of ~s objs -> ~s methods" cl-name
	      ;;var-count obj-count other-count)

	      (cond ((has-spres-flag? :simple-package)
		     ;; just spit it out if it's simple
		     (present-objs-in-package doc obj normal-objs :general :refsect1)
		     (present-objs-in-package doc obj var-objs :vars :refsect1))

		    (var-objs ;; hack
		     (put doc "   <refentry id=\"packageX" (make-valid-entity cl-name) "Xvariables\">" (eol))
		     (put doc "<refnamediv>" (eol)
			  "<refname>" cl-name " variables</refname>" (eol)
			  "<refpurpose>All variables and constants</refpurpose>"
			  "</refnamediv>" (eol))
		     (present-objs-in-package doc obj var-objs :vars :refsect1)
		     
		     (put doc " </refentry>" (eol))
		     
		     ;; --

		     (put doc "   <refentry id=\"packageX" (make-valid-entity cl-name) "Xcontent\">" (eol))
		     (put doc "<refnamediv>" (eol)
			  "<refname>" cl-name " full listing</refname>" (eol)
			  "<refpurpose>" "All funcallable objects" "</refpurpose>"
			  "</refnamediv>" (eol))
		     (present-objs-in-package doc obj normal-objs :general :refsect1)
		     		    
		     (put doc " </refentry>" (eol))
		     )
		    
		    ;; if we're not so simple
		    (t
		     (put doc "   <refentry id=\"packageX" (make-valid-entity cl-name) "Xcontent\">" (eol))
		     (put doc "<refnamediv>" (eol)
			  "<refname>" cl-name " full listing</refname>" (eol)
			  "<refpurpose>All funcallable objects and all variables</refpurpose>"
			  "</refnamediv>" (eol))
		     (present-objs-in-package doc obj normal-objs :general :refsect1)
		     (present-objs-in-package doc obj var-objs :vars :refsect1)

		     (put doc " </refentry>" (eol))))
	      ) ;; end special doc
	      )) ;; end normal-objs
	  
	  (when (has-spres-flag? :simple-package)
	    (put doc "</refentry>" (eol))
	    (remove-spres-flag! :simple-package))
	  
	  (put doc "</reference>" (eol))
	  
	  ))))
