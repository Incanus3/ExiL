;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CSF -*-

#|

DESC: specs/csf-extra.lisp - extra functions for the CSF API
Copyright (c) 1998,1999 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :sds-api-csf)

(defmethod equal-to ((a csf-location) (b csf-location))
  ;;  (warn "comparing ~a and ~a" a b)
  (when (equal (csf-location.file a) (csf-location.file b))
    (when (equal (csf-location.startline a) (csf-location.startline b))
      (when (equal (csf-location.endline a) (csf-location.endline b))
	(return-from equal-to t))))
  nil)


(defmethod equal-to ((a csf-where) (b csf-where))
  ;; assuming only one loc
  (equal-to (car (csf-where.location a))
	    (car (csf-where.location b))))

;; fix later
(defmethod equal-to ((a csf-class) (b csf-class))
  (when (equal (csf-class.name a) (csf-class.name b))
    (return-from equal-to t))
  nil)

(defmethod equal-to ((a csf-method) (b csf-method))
  (when (equal (csf-method.name a) (csf-method.name b))
    (return-from equal-to t))
  nil)

(defmethod equal-to ((a csf-variable) (b csf-variable))
  (when (equal (csf-variable.name a) (csf-variable.name b))
    (when (equal-to (car (csf-variable.location a)) 
		    (car (csf-variable.location b)))
      (return-from equal-to t)))
  nil)

(defmethod equal-to ((a csf-typespec) (b csf-typespec))
  (when (equal (csf-typespec.name a) (csf-typespec.name b))
    (when (equal-to (car (csf-typespec.location a)) 
		    (car (csf-typespec.location b)))
      (return-from equal-to t)))
  nil)

(defmethod equal-to ((a csf-enum) (b csf-enum))
  (when (equal (csf-enum.name a) (csf-enum.name b))
    (when (equal-to (car (csf-enum.location a)) 
		    (car (csf-enum.location b)))
      (return-from equal-to t)))
  nil)

(defmethod equal-to ((a csf-info) (b csf-info))
  
  (when (and (equal (csf-info.type a) (csf-info.type b))
	     (equal (csf-info.value a) (csf-info.value b))
	     (equal (csf-info.info a) (csf-info.info b)))
    (return-from equal-to t)))



(defmethod equal-to ((a csf-comment) (b csf-comment))
  (when (equal-to (car (csf-comment.location a)) 
		  (car (csf-comment.location b)))
    (when (equal-to (car (csf-comment.text a)) 
		    (car (csf-comment.text a)))
      (return-from equal-to t)))
  nil)

(def-or-method get-object-name ((obj (or csf-class
					 csf-package
					 csf-method
					 csf-typespec
					 csf-enum
					 csf-directive
					 csf-variable)))
  (car (slot-value obj 'name)))


(def-or-method get-object-id ((obj (or csf-class
				       csf-package
				       csf-method
				       csf-typespec
				       csf-enum
				       csf-variable)))
  (car (slot-value obj 'id)))


(def-or-method print-object ((inst (or csf-method
				       csf-class
				       csf-package
				       csf-directive
				       csf-variable)) stream)
  (print-unreadable-object (inst stream :identity t)
			   (format stream "~:(~S~) [~S]" (class-name (class-of inst)) (get-object-name inst)))
  inst)

(defmethod print-object ((inst csf-where) stream)
  (print-unreadable-object (inst stream :identity t)
			   (format stream "~:(~S~) [~S]" (class-name (class-of inst)) (csf-where.location inst)))
  inst)

(defmethod print-object ((inst csf-location) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (class-name (class-of inst)) 
	   (csf-location.file inst)
	   (csf-location.startline inst)))
  inst)


(defmethod print-object ((inst csf-info) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S§~S§~S]" (class-name (class-of inst)) 
	   (slot-value inst 'type)
	   (slot-value inst 'value)
	   (slot-value inst 'info)))
  inst)

(defmethod do-iteration ((node csf-toplevel) function context 
			 &key (recursive t) &allow-other-keys)
  (when recursive
    (dolist (x (slot-value node 'content))
      (funcall function x context :in))))

(defmethod do-iteration ((node csf-class) function context 
			 &key (recursive t) &allow-other-keys)
  (when recursive
    (dolist (x (slot-value node 'content))
      (funcall function x context :in))))

(defmethod do-iteration ((node csf-package) function context 
			 &key (recursive t) &allow-other-keys)
  (when recursive
    (dolist (x (slot-value node 'content))
      (funcall function x context :in))))


(defstruct (csf-buckets (:conc-name csf-buckets.))
  packages
  comments
  classes
  methods
  vars
  typespecs
  enums
  directives)

(defun bucket-sort-csf-objs (obj-list)
  "Sorts obj-list into an object of type csf-buckets and
returns it."
  (let ((buckets (make-csf-buckets)))
    
    (dolist (i obj-list)
      (typecase i
	(csf-package   (push i (csf-buckets.packages buckets)))
	(csf-class     (push i (csf-buckets.classes buckets)))
	(csf-comment   (push i (csf-buckets.comments buckets)))
	(csf-method    (push i (csf-buckets.methods buckets)))
	(csf-typespec  (push i (csf-buckets.typespecs buckets)))
	(csf-enum      (push i (csf-buckets.enums buckets)))
	(csf-variable  (push i (csf-buckets.vars buckets)))
	(csf-directive (push i (csf-buckets.directives buckets)))
	(t
	 (warn "Don't know where to push ~a" i))
	))

    buckets))


(defun find-patchable-obj (obj candidates)
  "Tries to find an object to patch obj vs from candidates.
Returns that object when found and NIL when no object was found.
Uses EQUAL-TO to find object."
  (find obj candidates :test #'equal-to))
  

(defgeneric patch-csf-obj (orig-obj new-obj)
  (:documentation "Patches the original object with info
from the new object.  This means that the first argument
may be modified."))

(defmethod patch-csf-obj (orig-obj new-obj)
  (warn "No handler written for (PATCH-CSF-OBJ ~a ~a)"
	(its-name orig-obj)
	(its-name new-obj))
  :no-handler)

(defmethod patch-csf-obj ((orig-obj csf-class) (new-obj csf-class))
  (warn "Patching class ~a with ~a" 
	(get-object-name orig-obj) 
	(get-object-name new-obj))
  ;; id probably doesn't need to be updated for a class
  ;; name should be stable
  ;; location should also be stable
  ;; access as well
  ;; parents should remain the same
  
  ;; we add new info
  (let ((old-info (csf-class.info orig-obj)))
    (dolist (x (csf-class.info new-obj))
      (let ((other (find x old-info :test #'equal-to)))
	(unless other
	  (warn "adding info..")
	  (push x (csf-class.info orig-obj))))))
  
  ;;  (warn "~a <-> ~a" (slot-value orig-obj 'content) (slot-value new-obj 'content))
  
  ;; we patch content
  (let ((old-content (slot-value orig-obj 'content)))
    (dolist (x (slot-value new-obj 'content))
      (let ((other (find x old-content :test #'equal-to)))
	(when other
	  (patch-csf-obj other x)))))

  :patched)

(defmethod patch-csf-obj ((orig-obj csf-comment) (new-obj csf-comment))
  :no-patch-needed)

(defmethod patch-csf-obj ((orig-obj csf-typespec) (new-obj csf-typespec))
  :no-patch-needed)

(defmethod patch-csf-obj ((orig-obj csf-enum) (new-obj csf-enum))
  :no-patch-needed)

(defmethod patch-csf-obj ((orig-obj csf-variable) (new-obj csf-variable))
  (warn "Patching var ~a with ~a" 
	(get-object-name orig-obj) 
	(get-object-name new-obj))
  
  ;; id should remain, no?
  ;; name should not be changed
  ;; fixing locations
  (let ((old-locs (csf-variable.location orig-obj)))
    (dolist (x (csf-variable.location new-obj))
      (warn "checking loc ~a" x)
      (let ((other (find x old-locs :test #'equal-to)))
	(unless other
	  (warn "adding loc to ~a" (get-object-name orig-obj) )
	  (push x (csf-variable.location orig-obj))))))

  ;; fixing info
  (let ((old-info (slot-value orig-obj 'info)))
    (dolist (x (slot-value new-obj 'info))
      (let ((other (find x old-info :test #'equal-to)))
	(unless other
	  (warn "adding info..")
	  (push x (slot-value orig-obj 'info))))))
 
  
  :patched)

(defmethod patch-csf-obj ((orig-obj csf-method) (new-obj csf-method))
  (warn "Patching meth ~a with ~a" 
	(get-object-name orig-obj) 
	(get-object-name new-obj))
  
  ;; id should be updated
  ;; name should stay
  ;; fixing locations
  (let ((old-locs (csf-method.where orig-obj)))
    (dolist (x (csf-method.where new-obj))
      ;;      (warn "checking loc ~a" x)
      (let ((other (find x old-locs :test #'equal-to)))
	(unless other
	  ;;	  (warn "adding loc to ~a" (get-object-name orig-obj) )
	  (push x (csf-method.where orig-obj))))))
  
  ;; fixing info
  (let ((old-info (slot-value orig-obj 'info)))
    (dolist (x (slot-value new-obj 'info))
      (let ((other (find x old-info :test #'equal-to)))
	(unless other
	  (warn "adding info..")
	  (push x (slot-value orig-obj 'info))))))
  
  ;; retvals
  ;; args
   
  :patched)


(defun parse-csf-file (fname)
  "Returns the top-objects or NIL"
  
  (parse-typed-xml-file fname (make-csf-factory) "CSF"))

