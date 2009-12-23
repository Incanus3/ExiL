;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/rules/sort.lisp - sorting of content for presentation
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres-impl)

#||
(def-rule-info
    ((:key :fallback-content-presenter)
     (:name print-content-list)
     (:req ;;(?class sdoc-class)
	   ;;(?document-type docbook-document)
	   ))

    
    (let ((the-list content-list))
      (dolist (i the-list)
	(present-object i doc ?context)))
  )

(def-rule-info
    ((:key :sorted-content-presenter)
     (:name print-content-list)
     (:req ;;(?class sdoc-class)
	   ;;(?document-type docbook-document)
	   (eq ?list-style :sorted)))
    
    (let ((the-list (tl-sort-by-name content-list)))
      (dolist (i the-list)
	(present-object i doc ?context)))
  )
||#

(def-rule-info
    ((:key :clever-sort-of-content-presenter)
     (:name print-content-list)
     (:req (eq ?list-style :clever-sort))
     (:desc "A positively evil way to sort content.")
     (:variation :never-print-execute))

    ;; first we find objects requiring individual files
    (multiple-value-bind (normal-objs individual-files)
	(filter-away content-list #'(lambda (x)
				      (should-have-individual-file-p x nil)))

      (when individual-files
	(error "Have got individual file ~s, this should be fixed elsewhere"
	       individual-files))
      
    (let ((buckets (tl-divide-and-order-objects normal-objs
						?prog-lang
						:also-sort t)))
	  

      ;; filter individual-files later, merge should be ok
      (let ((merged-buckets (loop for x in buckets
				  collecting
				  (when x
				    (tl-merge-list doc x))))
	    (next-state (tl-get-next-section-state ?rec-state)))


	(dolist (i merged-buckets)
	  (when (and i (car i))
	    ;; fix this later to depend on rec-state above
	    (put doc "<" (cdr next-state) ">" (eol)
		 "<title class=\"contenttitle\">"
		 (get-word (tl-get-sect-heading-from-type (car i)) doc)
		 "</title>" (eol))
	    
	    (let ((?rec-state (car next-state)))
	      (dolist (x i)
		(assert (eq nil (should-have-individual-file-p x nil)))
		(present-object x doc ?context)))
	    
	    (put doc "</" (cdr next-state) ">" (eol))))
	      
	))))

#||
(def-rule-info
    ((:key :lispy-pack-sort-content-presenter)
     (:name print-content-list)
     (:req (eq ?list-style :lisp-pack-sort)
	   (eq ?prog-lang :lisp))
     (:desc "A positively evil way to sort content."))

    ;; Use some of the cooler general features above..
    (multiple-value-bind (normal-objs individual-files)
	(filter-away content-list #'(lambda (x)
				      (should-have-individual-file-p x nil)))
      
      (when individual-files
	(error "Have got individual file ~s, this should be fixed elsewhere"
	       individual-files))

      (let ((buckets (tl-divide-and-order-objects normal-objs
						  ?prog-lang
						  :also-sort t)))
	  
	
	;; filter individual-files later, merge should be ok
	(let ((merged-buckets (loop for x in buckets
				    collecting
				    (when x
				      (tl-merge-list doc x))))
	      (next-state (tl-get-next-section-state ?rec-state)))

	(dolist (i merged-buckets)
	  (when (and i (car i))
	    ;; fix this later to depend on rec-state above
	    (put doc "<" (cdr next-state) ">" (eol)
		 "<title class=\"contenttitle\">"
		 (get-word (tl-get-sect-heading-from-type (car i)) doc)
		 "</title>" (eol))
	    
	    (let ((?rec-state (car next-state)))
	      (dolist (x i)
		(assert (eq nil (should-have-individual-file-p x nil)))
		(present-object x doc ?context)))
	    
	    (put doc "</" (cdr next-state) ">" (eol))))
	      
	))))
||#
	  #||
	  
    ;; we wish to organise things 
    (let ((classes nil)
	  (meths nil)
	  (vars nil)
	  (rest nil))
      
      (dolist (x content-list)
	(typecase x
	  (sdoc-class (push x classes))
	  (sdoc-method (push x meths))
	  (sdoc-variable (push x vars))
	  (otherwise
	   (push x rest))))

      (when classes
	(let ((?rec-state :refsect1))
	  (dolist (i classes)
	    (present-object i doc ?context))))

;;	(warn "not presenting classes [~a]" (mapcar #'get-object-name classes)))

      ;; we want to presort things
      (flet (     
	      
	     (pres-list (the-list word)
	       (when the-list
		 (put doc "<refsect2 id=\"" (make-valid-entity (get-object-name ?obj))  +id-word-delim+
		      (make-valid-entity word) "\"><title>" (get-word word doc) "</title>" (eol))
		 (let ((sorted-objs (tl-sort-by-name the-list)))
		   (dolist (i sorted-objs)
		     (present-object i doc ?context)))
		 (put doc "</refsect2>" (eol)))
	       (values)))

	(pres-list vars "Variables")
		
	(setf meths (tl-categorise-duplicates meths :type "method"))
	(pres-list meths "Functions et.al")
	(pres-list rest "Other content."))
      
      (values))
    ))

||#