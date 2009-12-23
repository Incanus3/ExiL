;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: albert -*-

#|

DESC: tools/linker.lisp - the CSF Linker
Copyright (c) 1998-2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :albert)


(defun merge-csf-info (first-tree-list second-tree-list)
  "Merges the contents of two tree-lists (top-objects),
and lets the first have the correct info.  Returns the
new top-level in a list." 

  (assert (= (length first-tree-list) 1))
  (assert (= (length second-tree-list) 1))
  
  (let* ((base-top (car first-tree-list))
	 (new-top (car second-tree-list))
	 (buckets (bucket-sort-csf-objs (csf-toplevel.content base-top))))
    
    (when-verbose
	(format t "Merging ~a and ~a~%" base-top new-top))
         
    ;; we ignore lang ever so long
    (let ((new-content (csf-toplevel.content new-top)))
      (dolist (i new-content)
	(typecase i
	  (csf-package  (find-and-patch-csf-obj i (csf-buckets.classes   buckets) base-top))
	  (csf-class    (find-and-patch-csf-obj i (csf-buckets.classes   buckets) base-top))
	  (csf-method   (find-and-patch-csf-obj i (csf-buckets.methods   buckets) base-top))
	  (csf-typespec (find-and-patch-csf-obj i (csf-buckets.typespecs buckets) base-top))
	  (csf-enum     (find-and-patch-csf-obj i (csf-buckets.enums     buckets) base-top))
	  (csf-variable (find-and-patch-csf-obj i (csf-buckets.vars      buckets) base-top))
	  (csf-comment  (find-and-patch-csf-obj i (csf-buckets.comments  buckets) base-top))
	  (t (warn "Unknown CSF Object ~a" i)))))
        
    
    (list new-top)))

(defun find-and-patch-csf-obj (obj candidates base-top)
  "Returns result of (patch-csf-obj ..) when an object was patched 
and :not-patched if it wasn't."
  (let ((patchable-obj (find-patchable-obj obj candidates)))
    (if patchable-obj
	(patch-csf-obj patchable-obj obj)
	(progn
	  (push obj (csf-toplevel.content base-top)) ; we're new
	  :added))))

(defun link-csf-files (file-list out-file)

  (when (< (length file-list) 2)
    (warn "You only gave one CSF-file. That's hardly a challenge. You can do better.")
    (return-from link-csf-files (values)))

  ;; fix this..
  (let* ((csf-factory (make-csf-factory))
	 (base-csf-xml-tool (apispec-xml:make-xml-tool csf-factory)))

    (flet ((normalise-fname (fname)
	     (merge-pathnames (pathname fname))))

    ;; we need the first file for the base
    (let ((base-file (normalise-fname (car file-list))))
      
      (require-file base-file "CSF-Data")
      (let ((retval (apispec-xml:parse-xml (namestring base-file) base-csf-xml-tool))) ; side effect
	(unless retval
	  (warn "Something screwed up when parsing the CSF-file ~a" base-file))))

    ;; now let's take the rest of the list and link with base

    ;; fix this to use the normal function
    (dolist (cur-file (cdr file-list))
      (setq cur-file (normalise-fname cur-file))
      (require-file cur-file "CSF-Data")
      (let* ((tmp-xml-tool (apispec-xml:make-xml-tool csf-factory))
	     (retval (apispec-xml:parse-xml (namestring cur-file) tmp-xml-tool)))
	(unless retval
	  (warn "Something screwed up when parsing the CSF-file ~a" cur-file))

	(merge-csf-info (apispec-xml:xml-tool.top-objects base-csf-xml-tool)
			(apispec-xml:xml-tool.top-objects tmp-xml-tool))))
    
    ;; we should have the result by now.. 
    (with-open-file (str (normalise-fname out-file)
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format str "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>~%")
      (format str "<!DOCTYPE sdoc SYSTEM \"csf.dtd\">~%")
    
      (apispec-xml:print-as-xml (car (apispec-xml:xml-tool.top-objects base-csf-xml-tool))
				str base-csf-xml-tool))))
  
  (values))

