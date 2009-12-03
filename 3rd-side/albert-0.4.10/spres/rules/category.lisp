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
    ((:key :full-category/docbook)
     (:name present-object)
     (:desc "Presents a full category.")
     (:req (?class sdoc-category)
	   (?document-type docbook-document)
	   (?context (eql :full))
;;	   (eq ?prog-lang :java)
	   ;; assertion, we should be below a class, ie a refsect1
	   
	   )
     (:variation
      :never-print-execute))

;;    (unless (eq ?rec-state :refsect1)
;;      (error "A category found in an odd place..  ~a" ?rec-state))
    
    (let ((meth-name (get-object-name obj))
	  (the-type (get-string sdoc-category.type obj))
	  (the-parent ?parent)
	  (content-list (slot-value obj 'content)))
    
      (unless meth-name
	(setq meth-name "{not-named}"))


      (cond ((equal the-type "method")
	     #||
	     (when (string-equal meth-name "PRESENT-OBJECT")
	       (warn "PRESENT-OBJECT ~s vs ~s vs ~s ~s" obj ?parent (has-spres-flag? :related-methods) (get-enclosing-package obj)))
	     ||#
	     (let ((first-obj (first content-list)))
	       (put doc "<formalpara>" (eol)
		    "<title>"
;;		    (get-simple-anchor doc (make-obj-id doc first-obj the-parent))
		    (cond ((has-spres-flag? :related-methods)
			   (let ((up-package (get-enclosing-package obj)))
			     (if up-package
				 (concatenate 'string (get-object-name up-package) ":")
				 "")))
			  (t
			   ""))
		    meth-name
		    "</title>"
		    (eol))))
	    ((equal the-type "variable")
	     ;; do nothing..
	     )
	    ((equal the-type "userspec")
	     (error "USER-spec cats, not working yet")
	     ;;(tl-make-anchor "cat" +id-word-delim+
	     ;;(make-valid-entity meth-name) (format nil "~a" (random 100))
	     )
	    
	    (t
	     (warn "Fell through category-type with ~s." the-type)))
	     
		    
      (put doc "<para>" (eol))
      
      (unless (has-spres-flag? :dont-print-anchor)
	;; hack to add all anchors in right place
	(when (or (equal the-type "variable")
		  (equal the-type "method"))
	  
	  (let ((good-ids (remove-duplicates (loop for i in content-list
						   collecting (make-obj-id doc i the-parent))
					     :test #'equal)))
	    (dolist (i good-ids)
	      (put doc (get-simple-anchor doc i))))))
      
;;      (warn "Writing out ~a" (tl-get-cat-info-str obj))
      
      (put doc "<programlisting>")

      ;; FIX this to include all info.
      ;; hackisj
      (dolist (i content-list)
	(etypecase i
	  (sdoc-method
	   (put doc (apispec-xml:xmlify-string (get-method-signature doc i :linked nil :style :intuitive)) (eol)))
	  (sdoc-variable
	   (let ((exported? (is-exported? i (get-object-name i))))

	     (if exported?
		 (put doc "<property role=\"exported\">")
		 (put doc "<property role=\"internal\">"))
	     (put doc (apispec-xml:xmlify-string (get-variable-signature doc i :linked nil :style :intuitive)))
	     (put doc "</property>")
	     (put doc (eol))
	     ))))
      
      (put doc "</programlisting>" (eol))
            
      (put doc "</para>" (eol))
      
      (when (or (equal the-type "method")
		(equal the-type "userspec"))
	(put doc "</formalpara>" (eol)))
	   
  ))
