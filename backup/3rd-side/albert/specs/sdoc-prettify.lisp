;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SDOC -*-

#|

DESC: specs/sdoc-prettify.lisp - code to prettify sdoc-structures
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :sds-api-sdoc)

(defmethod prettify-tree ((object sdoc-toplevel) parent-list)
  
  (declare (ignore parent-list))
  
  (let ((*prettification-language* nil)
	(lang (car (sdoc-toplevel.language object))))
    
    (cond ((string-equal lang "java") (setq *prettification-language* :java))
	  ((string-equal lang "c++") (setq *prettification-language* :c++))
	  ((string-equal lang "lisp") (setq *prettification-language* :lisp))
	  (t
	   (warn "Unknown language ~a in tree that is to be prettified"
		 lang)))
    
    (let ((content-list (sdoc-toplevel.content object))
	  (obj-table (make-hash-table :test #'equal))
	  (other-objs nil))
      
      (dolist (x content-list)
	(cond ((typep x 'sdoc-package)
	       (push x (gethash (get-object-name x) obj-table)))
	      (t
	       (push x other-objs))))
      
      (maphash #'(lambda (k v)
		   (declare (ignore k))
;;		   (warn "Checking ~a ~a" k v)
		   (let ((obj (if (cdr v) (merge-tree-list v) (car v))))
		     (push obj other-objs)))
	       obj-table)
      
      (let ((par-list (list object)))
	(setq other-objs (mapcar #'(lambda (x) (prettify-tree x par-list))
				 other-objs)))
      
;;      (when-verbose 
;;       (warn "Assigning content-list ~a" other-objs))
      
      (setf (sdoc-toplevel.content object) other-objs)))

  object)

(defmethod merge-trees ((first sdoc-package)
			(second sdoc-package))
  
  ;; right now, it doesn't really matter how we do it.. we just 
  ;; add together the lists
  
  ;; what to do with the info-list?
  
  (setf (sdoc-package.location first) nil) ; not needed.. or?
  (setf (sdoc-package.content first) (nconc (sdoc-package.content first)
					    (sdoc-package.content second)))
  
  (setf (sdoc-package.doc first) (nconc (sdoc-package.doc first)
					(sdoc-package.doc second)))

  
  
;;  (warn "Merging packages ~a and ~a" first second)
  first)

