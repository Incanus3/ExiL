;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/rules/basic.lisp - the base-rules and unplaced rules
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres-impl)

(def-rule-form print-header   ((?class obj ?obj)
			       (?document-type doc ?doc)
			       (?context context ?context)))

(def-rule-form present-object ((?class obj ?obj)
			       (?document-type doc ?doc)
			       (?context context ?context))
  (:error-on-fall-through))

(def-rule-form get-file-extension  ((?document-type doc ?doc))
  (:never-print-execute))

(def-rule-form print-purpose ((?class obj ?obj)
			      (?document-type doc ?doc)
			      (?context context ?context))
  (:never-print-execute))

(def-rule-form print-content-list ((?class obj ?obj)
				   (?document-type doc ?doc)
				   (?list content-list))
  (:warn-on-fall-through))
  

(def-rule-info
    ((:key :document-header/docbook)
     (:name print-header)
     (:desc "Prints the header of the document.")
     (:req (?document-type docbook-document)
	   (?context (eql :document))))
  
  (put doc "<?xml version='1.0'?>" (eol))
  (put doc "<!DOCTYPE book PUBLIC \"-//Norman Walsh//DTD DocBk XML V3.1.7//EN\"
               \"" (db-get-dtd) "\" " )

  (when (and ?file-table (> (hash-table-count ?file-table) 0))
    
    (put doc "[" (eol))

    (loop for val being the hash-values of ?file-table
	  for fname = (file-info-to-fname val)
	  do
	  (put doc
	       "<!ENTITY "
	       "fileX" (make-valid-entity fname) " "
	       "SYSTEM \"" fname (get-file-extension doc)
	       "\">" (eol)))
    (put doc "]"))
  
  (put doc ">" (eol)))


(def-rule-info
    ((:key :book-header/docbook)
     (:name print-header)
     (:desc "Prints a book header for docbook.")
     (:req (?document-type docbook-document)
	   (?context (eql :book))
	   ))
    
    (let* ((short-name (setting-or-default '("system" "name") ""))
	   (info-list '())
	   (authors (list (setting-or-default '("system" "author" "name") "Unknown Author"))))

      ;; hack!
      (when (every #'lower-case-p short-name) 
	(setf short-name (format nil "~@(~a~)" short-name)))
      
      (put doc "<title>" short-name " Reference Documentation</title>" (eol))
      
      (put doc "<bookinfo>" (eol))
      
      (put doc "<productname>" short-name "</productname>" (eol))
      
      (let ((date (albert-setting '("system" "date")))
	    (version (setting-or-default '("system" "version" "number") "v1.0")))
	(unless date
	  (setf date (sds-global:get-date-string-for-today)))
	(unless date
	  (setf date "2003"))
	
	(put doc "<productnumber>" version "</productnumber>" (eol))
	(put doc "<pubdate>" date "</pubdate>" (eol)))

      (when-bind (licencefile (albert-setting '("system" "licencefile")))
	(when (or (pathnamep licencefile)
		  (and (stringp licencefile)
		       (plusp (length licencefile))))

	  (cond ((not (probe-file licencefile))
		 (albert-warn "docbook> unable to find Legal Notice file ~s" licencefile))
		(t
		 (with-open-file (in-str licencefile :direction :input)    
		   (put doc "<legalnotice> <title>"
			(albert-setting '("albert" "presentation" "legalnotice" "title"))
			"</title><programlisting><![CDATA[" (eol))

	           (loop for x = (read-line in-str nil 'eof)
			 until (eq x 'eof)
			 do
			 (progn
			   ;; clean any evil chars
			   (loop for i from 0
				 for ch across x
				 do
				 (cond ((= (char-code ch) 12)
					(setf (schar x i) #\Space))
				       (t nil)))
			   (put doc x (eol))))
		   
		   (put doc (eol) "]]> </programlisting></legalnotice>"))
		 ))))
      
      ;; skip authors.. make separate function
      
      (put doc "</bookinfo>" (eol))
      
      t))


(def-rule-info
    ((:key :tpl/docbook)
     (:name present-object)
     (:desc "Toplevel Docbook basically")
     (:req (?class sdoc-toplevel)
	   (?document-type docbook-document)
	   (?context (eql :full))))

  (when-verbose
      (albert-info "docbook> presenting toplevel to ~a" ?doc))

  (when-bind (better-dtd (albert-setting '("albert" "docbook" "dtd")))
    (setf (db-get-dtd) better-dtd))
  
  ;;(print-header obj doc :document)
  
  ;;(put doc "<book>" (eol))
  (print-header obj doc :book)
  
  (let* ((content (slot-value obj 'content))
	 (indexable-stuff (notevery #'is-empty? content))
	 (num-classes (hierarchy-size ?class-hierarchy))
	 (hier-setting (albert-setting '("albert" "presentation" "index" "class-hierarchy")))
	 (include-hier (cond ((eq hier-setting t) t)
			     ((integerp hier-setting)
			      (<= hier-setting num-classes))
			     (t nil))))
	 
   
    (dolist (i content)
      ;;(warn "TPL: checking ~a" i)
      (unless (is-empty? i)
	(present-object i doc context)))

    (when (or (and indexable-stuff
		   (albert-setting '("albert" "presentation" "index" "global-index")))
	      include-hier)
      ;; time to do an index
      (put doc "<reference id=\"indexReference\">" (eol))
      
      (put doc "  <title>" (get-word "Indexes" doc) "</title>" (eol))

      ;;(warn "Hierarchy size for ~s is ~s" ?doc (hierarchy-size ?class-hierarchy))
      (when include-hier
				 
	(when-verbose
	    (albert-info "spres> writing class-hierarchy to book."))

	(register-separate-document (make-file-info :id "class-hierarchy"
						    :dir nil
						    :fname (make-valid-entity "class-hierarchy")))
	(with-ok-document (doc "class-hierarchy" doc)
	  (put doc "<refentry id=\"classHierarchy\">" (eol))
	  
	  (put doc "<refnamediv>"
	       "<refname>" (get-word "Class Hierarchy" doc) "</refname>"
	       "<refpurpose>Clickable index of all classes</refpurpose>"
	       "</refnamediv>" (eol))
	  (put doc "<refsect1><title></title>" (eol))
	  (put doc "<programlisting>" (eol))
	  
	  (print-class-hierarchy doc ?class-hierarchy 0)
	  (put doc "</programlisting>" (eol))
	  (put doc "</refsect1>" (eol))
	  (put doc "</refentry>" (eol))
	  ))

      
      (when (and indexable-stuff
		 (albert-setting '("albert" "presentation" "index" "global-index")))
	;; time to do an index

	(register-separate-document (make-file-info :id "global-index"
						    :dir nil
						    :fname (make-valid-entity "global-index")))
	(with-ok-document (doc "global-index" doc)
	  
	  (put doc "<refentry id=\"globalIndex\">" (eol))
	  
	  (put doc "<refnamediv>"
	       "<refname>" (get-word "Global Index" doc) "</refname>"
	       "<refpurpose>Clickable index of all symbols</refpurpose>"
	       "</refnamediv>" (eol))
	  
	  (dolist (i content)
	    (unless (is-empty? i)
	      (present-object i doc :index)))
	  
	  (put doc "</refentry>" (eol))
	  )) ;; end glob index
      
      (put doc "</reference>" (eol))
      )

  ;;(put doc "</book>" (eol))
  ))

#||
  
      
      (put doc "<refnamediv>"
	   "<refname>" (get-word "Global Index" doc) "</refname>"
	 "<refpurpose></refpurpose>"
	 "</refnamediv>" (eol))

    
    ;; let's have a stab at the class-hierarchy
    (when (albert-setting '("albert" "presentation" "class-hierarchy"))
      (when-verbose
	  (albert-info "spres> writing class-hierarchy to book."))

      (put doc "<reference id=\"hierarchyReference\">" (eol))
      
      (put doc "  <title>" (get-word "Class Hierarchy" doc) "</title>" (eol))
      
      (put doc "<refentry id=\"classHierarchy\">" (eol))
      
      (put doc "<refnamediv>"
	   "<refname>" (get-word "Class Hierarchy" doc) "</refname>"
	   "<refpurpose></refpurpose>"
	   "</refnamediv>" (eol))
      (put doc "<programlisting>" (eol))

      (print-class-hierarchy doc ?class-hierarchy 0)
      (put doc "</programlisting>" (eol))
      (put doc "</refentry>" (eol))
      (put doc "</reference>" (eol)))
    
        
    (when indexable-stuff
      ;; time to do an index
      (put doc "<reference id=\"indexReference\">" (eol))
      
      (put doc "  <title>" (get-word "Various Indexes" doc) "</title>" (eol))
      
      (put doc "<refentry id=\"globalIndex\">" (eol))
      
      (put doc "<refnamediv>"
	   "<refname>" (get-word "Global Index" doc) "</refname>"
	 "<refpurpose></refpurpose>"
	 "</refnamediv>" (eol))

      (dolist (i content)
	(unless (is-empty? i)
	  (present-object i doc :index)))
    
      (put doc "</refentry>" (eol))
      (put doc "</reference>" (eol))
      ))

  
  (put doc "</book>" (eol)))
||#

(def-rule-info
    ((:key :file-extension/docbook)
     (:name get-file-extension)
     (:desc "returns a file-extension")
     (:req (?document-type docbook-document)))
    
    ".xml")
