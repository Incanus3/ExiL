;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/rules/class.lisp - the rules for classes
Copyright (c) 2000-2001,2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres-impl)


(def-rule-info
    ((:key :full-class/docbook)
     (:name present-object)
     (:desc "Presents a full class.")
     (:req (?class sdoc-class)
	   (?document-type docbook-document)
	   (?context (eql :full))
	   )
     (:variation :never-print-execute))

    ;; assertion, we should be below a package
    (assert (eq ?rec-state :reference))

  ;;  (warn "STRUCT-CHECK ~s for ~s" (is-struct? obj) (get-object-name obj))
  
  (let ((*scope-stack* (cons obj *scope-stack*)))

    ;; we're in java, we might find stuff needing it's own file
    (multiple-value-bind (content-list individual-files)
	(filter-away (slot-value obj 'content)
		     #'(lambda (x)
			 (should-have-individual-file-p x nil)))

      ;; any individual files are handled last

      
      ;; a class  should be in a separate file
      (with-ok-obj-document (doc obj doc)
	(let ((cl-name (get-object-name obj))
	      (print-class-details t)
	      (quickindex-num (setting-or-default '("albert" "presentation" "class" "quickindex") 7)))

	  (put doc "<refentry class=\"defclass\" id=\"" (make-obj-id doc obj ?parent) "\">" (eol))

	  (when-bind (page-title (docbook-page-title doc obj))
	    (when (and (stringp page-title)
		       (plusp (length page-title)))
	      (put doc " <refmeta><refentrytitle>" page-title "</refentrytitle></refmeta>" (eol))
	      ))
	  
	  (put doc " <refnamediv>" (eol)
	       "  <refname>" cl-name "</refname>" (eol))
      
	  (print-purpose obj doc context)

	  (put doc " </refnamediv>" (eol))
	  
	  (when-bind (doc-list (sdoc-class.doc obj))
	    (dolist (i doc-list)
	      (let ((type (slot-value i 'type))
		    (text (slot-value i 'text)))
		(when (string-equal (first type) "desc")
		  (put doc
		       " <refsynopsisdiv>" (eol)
		       "  <title class=\"contenttitle\">" (get-word "Description" doc) "</title>" (eol)
		       "  <simpara>" (first text) "</simpara>" (eol)
		       " </refsynopsisdiv>" (eol))))))
	      

	  (when (>= (length content-list) quickindex-num)
	    (put doc
		 " <refsect1>" (eol)
		 "  <title class=\"contenttitle\">"
		 (get-word "Quick-index" doc)"</title>" (eol))
	    (let* ((?parent obj))
	      (db-insert-index doc content-list 4 :only-link-existing nil))
	    (put doc
		 " </refsect1>" (eol)))

	  (when print-class-details
	    (db-present-class-details doc obj))

	  (add-spres-flag! :dont-print-location)
	  (let* ((?parent obj))
	    (present-with-content-manager obj doc content-list))
	  (remove-spres-flag! :dont-print-location)

	  (when (albert-setting '("albert" "presentation" "class" "related-methods"))
	    ;; do related methods, a bit slow method
	    (let* ((related (find-related-methods obj))
		   (result (set-difference related (slot-value obj 'content)
					   :test #'equal :key #'get-object-name)))
	      (when result
		(add-spres-flag! :related-methods)
		;;(add-spres-flag! :dont-print-location)
		(add-spres-flag! :dont-print-anchor)
		(let* ((?parent obj))
		  (present-with-content-manager obj doc result))
		(remove-spres-flag! :related-methods)
		;;(remove-spres-flag! :dont-print-location)
		(remove-spres-flag! :dont-print-anchor)
		)))
	  
	  
	  (put doc "</refentry>" (eol))
	  ))

      ;; ok, now let's dump any other stuff this class had hidden
      (when individual-files
	(let ((?parent obj))
	  (dolist (i individual-files)
	    (present-object i doc context))))
	   

      )))


(def-rule-info
    ((:key :generic-class-purpose)
     (:name print-purpose)
     (:req (?class sdoc-class)
	   (?document-type docbook-document))
     )

    (let ((purpose ""))

      (when-bind (doc-list (slot-value obj 'doc))
	(dolist (i doc-list)
	  (let ((type (slot-value i 'type))
		(text (slot-value i 'text)))
	    (when (string-equal (first type) "purpose")
	      (setf purpose (first text))))))
      

      #||
      (unless (plusp (length purpose))
	(let ((fields (tl-get-fields "documentation" obj)))
	  (when (and fields (stringp (first fields)))
	    (setf purpose (first fields)))))
      ||#

      (unless (plusp (length purpose))
	(let ((inh (list-to-sep-string (mapcar #'tl-make-link-for-class (get-inherit-obj obj))
				       :and-word (get-word "and" doc))))
	  (cond ((tl-is-struct? obj)
		 (setf purpose (albert-setting '("albert" "presentation" "default-purpose-string" "struct"))))
		(t
		 (setf purpose (albert-setting '("albert" "presentation" "default-purpose-string" "class")))))

	  (when (plusp (length inh))
	    (setf purpose (strcat purpose " inheriting " inh)))
	  
	  ))
      
      (when (eq purpose nil)
	(setf purpose ""))
      
      (put doc "  <refpurpose class=\"contenttitle\">" purpose "</refpurpose>" (eol))

      ))
