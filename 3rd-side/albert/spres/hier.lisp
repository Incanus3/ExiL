;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/hier.lisp - utilities for the class-hierarchy
Copyright (c) 1998,1999 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres-impl)

(defconstant +hierarchy-indent+ 4)

(defun %find-class-in-hierarchy (hier the-class)
  "Searches through given HIERARCHY hier for given class-name. Recursive"
  (block find-class-in-hierarchy
  ;;  (warn "going gung ~a" (if hier (car hier) hier))

  (unless (and hier the-class)
    (return-from find-class-in-hierarchy nil))
  
  (let ((class-name (etypecase the-class
		      (string the-class)
		      (sdoc-class (get-object-name the-class)))))


    (if (string= (get-object-name (tnode.class (car hier))) class-name)
	(return-from find-class-in-hierarchy (car hier))
	(if (tnode.kids (car hier))
	    (let ((res (%find-class-in-hierarchy (tnode.kids (car hier)) class-name)))
	      (when res
		(return-from find-class-in-hierarchy res)))))
    (%find-class-in-hierarchy (cdr hier) class-name)
    )))

(defun find-class-in-hierarchy (hier the-class)
  (let ((retval (%find-class-in-hierarchy hier the-class)))
    (unless retval
      (warn "searching for ~s gave ~s" the-class retval))
    retval))

(defun get-inherit-obj (class-obj)
  "Returns the INHERIT object in the given SDOC-CLASS"
  (sdoc-class.parents class-obj))

(defmethod print-class-hierarchy ((doc docbook-document) hier indent)
  "Currently prints the given hierarchy to given doc and assumes the
given doc is HTML."

  (if (null hier)
      nil
      (progn
	(put doc (make-string indent :initial-element #\Space) (tl-make-link-for-class (car hier)) (eol))
	(when (tnode.kids (car hier))
	  (print-class-hierarchy doc (tnode.kids (car hier)) (+ 2 indent)))
	  (print-class-hierarchy doc (cdr hier) indent)
	  )))

(defun prt-hier (hier &optional (indent 0))
  (cond ((null hier) nil)
	(t
	 (format t "~&~a~a~%" (subseq "                        " 0 indent)
		 (get-object-name (tnode.class (car hier))))
	 (when (tnode.kids (car hier))
	   (prt-hier (tnode.kids (car hier)) (+ 3 indent)))
	 (prt-hier (cdr hier) indent))))


(defun make-class-hierarchy (class-table) 
  "It creates and returns a class-hierarchy based on tree-node"
  
  (let ((entries nil))
 
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (dolist (x val)
		   (let ((id-obj (car x))
			 (act-obj (cdr x)))
		     ;; hack
		     (push (make-tree-node act-obj
					   :scope (sds-global::id-obj-scope id-obj))
			   entries))))
	     class-table)
  
    (dolist (x entries)
      ;;      (format t "Checking out class ~a~%" (get-object-name (tnode.class x)))
      (let ((inh (get-inherit-obj (tnode.class x))))
	(dolist (y inh)
	  (let* ((the-name (car (sdoc-inherit.name y)))
		 (found-class (if the-name
				  (find-class-in-hierarchy entries the-name)
				  nil)))
	    (when found-class
	      ;;		(format t "Found a class ~a~%" found-class)
	      (push x (tnode.kids found-class))
	      (push found-class (tnode.parents x)))))))

    (when-verbose
	(albert-info "spres> created class-hierarchy.."))

    ;; we need to filter out some stuff to make the tree cleaner
    (setf entries (filter #'(lambda (x) 
			      (unless (tnode.parents x)
				x))
			  entries))

    ;; at least sort the uppermost level
    (setf entries (sort entries #'string<
			:key #'(lambda (x)
				 (get-object-name (tnode.class x)))))
    
    entries))

(defun %get-indent (num)
  "returns indent as string"
  (make-string (+ num +hierarchy-indent+) :initial-element #\Space)) 


(defun draw-hierarchy (doc obj the-hierarchy)
  "Draws hierarchy somehow."
  
  (declare (ignore doc))
  
  (let* ((hier-obj (find-class-in-hierarchy the-hierarchy obj))
	 (inherits (sdoc-class.parents obj))
	 (kids (when hier-obj
		 (tnode.kids hier-obj))))

    (when hier-obj
      (when inherits
	
	(warn "Obj ~s inherits ~s" hier-obj inherits))
      (when kids
	(warn "Obj ~s has kids ~s" hier-obj kids)))

    ""))

(defun hierarchy-size (hier)
  "Returns an integer with an approximate size of the class-hierarchy."
  (labels ((count-subnodes (node)
	     (loop for i in (tnode.kids node)
		   summing (1+ (count-subnodes i)))))
    (cond ((consp hier)
	   (loop for i in hier
		 summing (1+ (count-subnodes i))))
	  ((eq hier nil) 0)
	  (t
	   (warn "Called hierarchy-size on ~s, dunno how, returning 0." hier)
	   0))))
