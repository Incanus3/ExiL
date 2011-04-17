;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CSF -*-

#|

DESC: specs/csf-prettify.lisp - code to prettify csf-structures
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :sds-api-csf)

(defmethod prettify-tree ((object csf-toplevel) parent-list)
  
  (declare (ignore parent-list))

;;  (when-verbose
;;      (warn "Prettifying tree.."))
  
  (let* ((*prettification-language* (figure-out-language object))
	 (content-list (csf-toplevel.content object))
	 (buckets (bucket-sort-csf-objs content-list))
	 (object-list nil))

;;    (when-verbose
;;	(warn "Prettifying tree.."))

    
    ;; first let us clear up in packages and classes
    (dolist (obj-list (list (arrange-duplicates (csf-buckets.packages buckets))
			    (arrange-duplicates (csf-buckets.classes buckets))
			    (arrange-duplicates (csf-buckets.methods buckets))
			    ))
	     
      (dolist (i obj-list)
	(push 
	 (if (consp i)
	     (merge-tree-list i)
	     i)
	 object-list)))

;;    (when-verbose
;;      (warn "Prettifying tree..~a" object-list))

    (setq object-list (nconc object-list
			     (csf-buckets.enums buckets)
			     (csf-buckets.comments buckets)
			     (csf-buckets.typespecs buckets)
			     (csf-buckets.vars buckets)))
			     

;;    (when-verbose
;;      (warn "Prettifying tree.."))
      
    (let ((par-list (list object)))
      (setq object-list (mapcar #'(lambda (x) (prettify-tree x par-list))
				object-list)))
      
;;      (when-verbose 
;;	  (warn "Assigning content-list ~a" object-list))
      
      (setf (csf-toplevel.content object) object-list))

  object)

(defmethod merge-trees ((first csf-package)
			(second csf-package))
  
  ;; right now, it doesn't really matter how we do it.. we just 
  ;; add together the lists
  
  ;; what to do with the info-list?
  
  (setf (csf-package.location first) nil) ; not needed.. or?
  
  (setf (csf-package.content first) (nconc (csf-package.content first)
					     (csf-package.content second)))
					   
  
;;  (warn "Merging packages ~a and ~a" first second)
  first)

(defmethod merge-trees ((first csf-class)
			(second csf-class))
  
  ;; right now, it doesn't really matter how we do it.. we just 
  ;; add together the lists and then get one step further..

  (dolist (slot (list 'location 'content 'info 'parents) 'access)
    (setf (slot-value first slot) (remove-duplicates (nconc (slot-value first slot)
							    (slot-value second slot))
						     :test #'equal-to)))

  first)

  

(defmethod prettify-tree ((object csf-package) parent-list)

  (declare (ignore parent-list))
  
  ;; make sure content is sorted
  (let ((content-list (csf-package.content object))
	(named-content nil)
	(other-content nil))
    (dolist (i content-list)
      (cond ((or (typep i 'csf-comment)
		 (typep i 'csf-directive))
	     (push i other-content))
	    ((typep i '(or csf-class csf-package csf-method csf-variable
			csf-enum csf-typespec))
	     (push i named-content))
	    (t
	     (error "Unknown content-type ~s in package ~s" i object))))
    
    (setq named-content (stable-sort named-content 
				     #'string-greaterp ;; wrong?
				     :key #'get-object-name))
    (setf (csf-package.content object)
	  (nconc named-content other-content))

    object))

(defmethod prettify-tree ((object csf-method) parent-list)
  (declare (ignore parent-list))
  
;;  (warn "Prettifying method..")
  
  object)


(defmethod merge-trees ((first csf-method) (second csf-method))

  ;; we just patch here and hope it is ok 
  (patch-csf-obj first second)

  first)
