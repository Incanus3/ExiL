;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/rules/variable.lisp - the rules for methods
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres-impl)


(def-rule-info 
    ((:key :full-variable/docbook)
     (:name present-object)
     (:desc "Presents a full variable.")
     (:req
      (?class sdoc-variable)
      (?document-type docbook-document)
      (?context (eql :full))
;;      (eq ?prog-lang :java)
      ;; assertion, we should be below a class, ie a refsect1
      ;; move later to a real assertion?
      ;;(eq ?rec-state :refsect1)
      )
     
     (:variation :never-print-execute)
     )
    
    ;; assertion, we should be below a class, ie a refsect1
    (unless (or (eq ?rec-state :refsect1)
		(eq ?rec-state :refsect2))
      (warn "Weird recstate for variable: ~s" ?rec-state)
      (return-from present-object nil))


    (let* ((obj-name (get-object-name obj))
	   (doc-list (slot-value obj 'doc))
	   (not-wanted (list "mod" "type"))
	   (info-list (get-info-except-types (slot-value obj 'info)
					     not-wanted))
	   (loc-list (slot-value obj 'location))
	   (show-loc (and loc-list (tl-show-location? :variable)))
	   (formal-pres nil)
	   (exportclass "")
	   (experiment t)
	   )

      (when (or doc-list info-list)
	(setf formal-pres t))
      
      (when (and show-loc (not (has-spres-flag? :dont-print-location)))
	(setf formal-pres t))

      (when (is-exported? obj obj-name)
	(setf exportclass " class=\"exported\""))

      
      (when (or formal-pres experiment)
	;; hack
	(unless obj-name
	  (setq obj-name "{not named}"))
	(put doc
	     " <formalpara" exportclass ">" (eol)
	     "  <title>"
	     (get-simple-anchor doc (make-obj-id doc obj ?parent))
	     obj-name
	     "  </title>" (eol)))
      
      (put doc "  <para>" (eol))
      (tree-put doc `(:programlisting nil
		      ,(get-variable-signature doc obj :linked nil :style :intuitive)))

      
      (when formal-pres
	(put doc
	     "   <variablelist>" (eol)
	     "    <title></title>" (eol))
	(db-present-doc-list  doc-list doc :variable :suppress-wrapper t)
	(unless (has-spres-flag? :dont-print-location)
	  (db-print-location loc-list doc :variable :suppress-wrapper t))
        (db-present-info-list info-list doc :variable :suppress-wrapper t)
        (put doc "   </variablelist>" (eol)))
      
      (put doc "  </para>" (eol))
      (when (or formal-pres experiment)
	(put doc " </formalpara>" (eol)))
      ))


;; this is a quick rip of the above rule, replace later
(def-rule-info 
    ((:key :full-typespec/docbook)
     (:name present-object)
     (:desc "Presents a full typespec.")
     (:req
      (?class sdoc-typespec)
      (?document-type docbook-document)
      (?context (eql :full))
      ;;(eq ?rec-state :refsect1)
      )
     (:variation
      :never-print-execute))

    ;; assertion, we should be below a class, ie a refsect1
    (unless (eq ?rec-state :refsect1)
      ;;(warn "Weird recstate for typespec: ~s" ?rec-state)
      (return-from present-object nil))

  
    (let* ((obj-name (get-object-name obj))
	   (doc-list (slot-value obj 'doc))
	   (not-wanted (list "mod" "type"))
	   (info-list (get-info-except-types (slot-value obj 'info)
					     not-wanted))
	   (loc-list (slot-value obj 'location))
	   (show-loc (and loc-list (tl-show-location? :variable)))
	   (formal-pres (or doc-list info-list show-loc))
	   )
		     

      (when formal-pres
	;; hack
	(unless obj-name
	  (setq obj-name "{not named}"))
	(put doc
	     "<formalpara>" (eol)
	     "<title>"
	     (get-simple-anchor doc (make-obj-id doc obj ?parent))
	     obj-name
	     "</title>" (eol)))
      
      (put doc "<para>" (eol))
      (tree-put doc `(:programlisting nil
		      ,(strcat "typespec " obj-name)))
;;		      ,(get-variable-signature doc obj :linked nil :style :intuitive)))

      
      (when formal-pres
	(put doc "<variablelist>" (eol))
	(db-present-doc-list  doc-list doc :variable :suppress-wrapper t)
	(db-print-location loc-list doc :variable :suppress-wrapper t)
        (db-present-info-list info-list doc :variable :suppress-wrapper t)
        (put doc "</variablelist>" (eol)))
      
      (put doc "</para>" (eol))
      (when formal-pres
	(put doc "</formalpara>" (eol)))
      ))


;; this is a rip of the above.. replace with something better later
(def-rule-info 
    ((:key :full-enum/docbook)
     (:name present-object)
     (:desc "Presents a full enum.")
     (:req
      (?class sdoc-enum)
      (?document-type docbook-document)
      (?context (eql :full))
      ;;(eq ?rec-state :refsect1)
      )
     (:variation
      :never-print-execute))

    ;; assertion, we should be below a class, ie a refsect1
    (unless (eq ?rec-state :refsect1)
      ;;(warn "Weird recstate for enum: ~s" ?rec-state)
      (return-from present-object nil))
  
    (let* ((obj-name (get-object-name obj))
	   (doc-list (slot-value obj 'doc))
;;	   (not-wanted (list "mod" "type"))
;;	   (info-list (get-info-except-types (slot-value obj 'info)
;;					     not-wanted))
	   (loc-list (slot-value obj 'location))
	   (show-loc (and loc-list (tl-show-location? :variable)))
	   (formal-pres nil) ;;(or doc-list info-list show-loc))
	   )
		     

      (when formal-pres
	;; hack
	(unless obj-name
	  (setq obj-name "{not named}"))
	(put doc
	     "<formalpara>" (eol)
	     "<title>"
	     (get-simple-anchor doc (make-obj-id doc obj ?parent))
	     obj-name
	     "</title>" (eol)))
      
      (put doc "<para>" (eol))
      (tree-put doc `(:programlisting nil
		      ,(strcat "enum " obj-name)))
;;		      ,(get-variable-signature doc obj :linked nil :style :intuitive)))

      
      (when formal-pres
	(put doc "<variablelist>" (eol))
	(db-present-doc-list  doc-list doc :variable :suppress-wrapper t)
	(db-print-location loc-list doc :variable :suppress-wrapper t)
;;        (db-present-info-list info-list doc :variable :suppress-wrapper t)
        (put doc "</variablelist>" (eol)))
      
      (put doc "</para>" (eol))
      (when formal-pres
	(put doc "</formalpara>" (eol)))
      ))


