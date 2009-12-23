;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SDOC -*-

#|

DESC: specs/sdoc-extra.lisp - extra functions for the SDOC API
Copyright (c) 1998,1999 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :sds-api-sdoc)

;;(defmethod-with-warn get-locations (obj))

(defmethod get-locations (obj)
  (warn "Unable to find get-loc for ~a" (its-name obj)))

(def-or-method get-locations ((obj (or sdoc-typespec
				       sdoc-enum
				       sdoc-directive
				       sdoc-variable
				       sdoc-package
				       sdoc-class)))
  (slot-value obj 'location))


(defmethod get-locations ((obj sdoc-method))
  (mapcar #'(lambda (x) (car (sdoc-where.location x))) 
	  (sdoc-method.where obj)))


(defmethod get-locations ((obj sdoc-category))
  ;; assumes info only have one member...
  nil)


(def-or-method get-object-name ((obj (or sdoc-class
					 sdoc-module
					 sdoc-category
					 sdoc-variable
					 sdoc-method
					 sdoc-directive
					 sdoc-typespec
					 sdoc-package
					 sdoc-inherit
					 sdoc-enum)))
  (car (slot-value obj 'name)))


(def-or-method get-object-id ((obj (or sdoc-class
				       sdoc-variable
				       sdoc-category
				       sdoc-method
				       sdoc-package
				       sdoc-typespec
				       sdoc-module
				       sdoc-enum)))
  (car (slot-value obj 'id)))



(defmethod equal-to ((a sdoc-location) (b sdoc-location))
  (when (equal (sdoc-location.file a) (sdoc-location.file b))
      (when (equal (sdoc-location.startline a) (sdoc-location.startline b))
	  (when (equal (sdoc-location.endline a) (sdoc-location.endline b))
		  (return-from equal-to t))))
  nil)


(defmethod equal-to ((a sdoc-where) (b sdoc-where))
  ;; ugly
  (equal-to (car (sdoc-where.location a)) 
	    (car (sdoc-where.location b))))
  
  
#||
(defmethod print-object ((inst sdoc-doc) stream)
  (print-unreadable-object (inst stream :identity t)
    (format stream "~:(~S~) [~S]" (class-name (class-of inst)) (get-object-name inst)))
  inst)
||#

(defun get-doc-as-pairs (doc-list)
  "Returns doclist as a list of pairs (type . text)"
  (mapcar #'(lambda (x)
	      (cons (car (sdoc-doc.type x)) (car (sdoc-doc.text x))))
	  doc-list))


(defmethod do-iteration ((node sdoc-toplevel) function context 
			 &key (recursive t) &allow-other-keys)
  (when recursive
    (dolist (x (slot-value node 'content))
      (funcall function x context :in))))

(defmethod do-iteration ((node sdoc-class) function context 
			 &key (recursive t) &allow-other-keys)
  (when recursive
    (dolist (x (slot-value node 'content))
      (funcall function x context :in))))

(defmethod do-iteration ((node sdoc-package) function context 
			 &key (recursive t) &allow-other-keys)
  (when recursive
    (dolist (x (slot-value node 'content))
      (funcall function x context :in))))

(defmethod do-iteration ((node sdoc-method) function context 
			 &key (recursive t) &allow-other-keys)
  (declare (ignore recursive))
  (funcall function node context :in))


(defmethod do-iteration ((node sdoc-typespec) function context 
			 &key (recursive t) &allow-other-keys)
  (declare (ignore recursive))
  (funcall function node context :in))


(def-or-method print-object ((inst (or sdoc-method
				       sdoc-class
				       sdoc-category
				       sdoc-module
				       sdoc-inherit
				       sdoc-variable
				       sdoc-package)) stream)
  (print-unreadable-object (inst stream :identity t)
    (format stream "~:(~S~) [~S]" (class-name (class-of inst)) (get-object-name inst)))
  inst)


(defmethod print-object ((inst sdoc-info) stream)
  (print-unreadable-object (inst stream :identity t)
    (format stream "~:(~S~) [~S§~S§~S]" (class-name (class-of inst)) 
	    (slot-value inst 'type)
	    (slot-value inst 'value)
	    (slot-value inst 'info)))
  inst)


(defun parse-sdoc-file (fname)
  "Returns the top-objects or NIL"
  
  (parse-typed-xml-file fname (make-sdoc-factory) "SDOC"))



(defun get-visibility (obj)
  "fetches the visibility from a parent obj.  return NIL
on failure."

  (let ((the-access (car (slot-value obj 'access))))
    (when the-access
      (return-from get-visibility 
	(car (sdoc-access.visibility the-access)))))
  nil)

(defun create-sdoc-module (name fullname)
  "Creates and returns a sdoc-module."

  (let ((mod (make-sdoc-module))
	(name-str (if (consp name) (car name) name))
	(fullname-str (if (consp fullname) (car fullname) fullname)))
	
    (setf (sdoc-module.id mod) (list (sds-global:%make-id+ "module" :name name-str
							   :fullname fullname-str)))
    (setf (sdoc-module.name mod) (force-to-list name))
    (setf (sdoc-module.fullname mod) (force-to-list fullname))

    mod))

;;(trace create-sdoc-module)

(defun is-generic-fun? (obj)
  (when (typep obj 'sdoc-method)
    (when-bind (type-list (get-info-of-type (sdoc-method.info obj) "mod"))
      (let ((presentable-type-list (mapcar #'caar (strip-info-fields type-list "type"))))
	(find "generic" presentable-type-list :test #'string=)))))

(defun is-method? (obj)
  (when (typep obj 'sdoc-method)
    (when-bind (type-list (get-info-of-type (sdoc-method.info obj) "mod"))
      (let ((presentable-type-list (mapcar #'caar (strip-info-fields type-list "type"))))
	(find "method" presentable-type-list :test #'string=)))))
