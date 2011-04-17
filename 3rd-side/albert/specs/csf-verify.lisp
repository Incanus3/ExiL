;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CSF -*-

#|

DESC: specs/csf-verify.lisp - code to verify csf-structures
Copyright (c) 1999 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :sds-api-csf)



(defmethod verify-object-actual ((obj csf-toplevel) context when)

  (declare (ignore when))
  ;; check language
  
  ;; then iterate over content
  (do-iteration obj #'verify-object-actual context))


(defmethod verify-object-actual ((obj csf-class) context when)

  (verify-id obj context)
  ;; check id and stuff
  (dolist (x (csf-class.info obj))
    (verify-object-actual x context when))
  ;; then iterate over content
  (do-iteration obj #'verify-object-actual context))


(defmethod verify-object-actual ((obj csf-package) context when)
  
  ;; check id and stuff
  (verify-id obj context)
  (dolist (x (csf-package.info obj))
    (verify-object-actual x context when))
  (dolist (x (csf-package.location obj))
    (verify-object-actual x context when))
  ;; then iterate over content
  (do-iteration obj #'verify-object-actual context)
  
  t)


(defmethod verify-object-actual ((obj csf-method) context when)

  ;; check id and stuff
  (verify-id obj context)
  
  ;; then iterate over content
  (dolist (x (csf-method.info obj))
    (verify-object-actual x context when))
    (dolist (x (csf-method.where obj))
    (verify-object-actual x context when))
  (dolist (x (csf-method.retvals obj))
    (verify-object-actual x context when))
  (dolist (x (csf-method.args obj))
    (verify-object-actual x context when))
  t)




(defmethod verify-object-actual ((obj csf-variable) context when)

  (verify-id obj context)
  (dolist (x (csf-variable.info obj))
    (verify-object-actual x context when))
  (dolist (x (csf-variable.location obj))
    (verify-object-actual x context when))
  t)


(defmethod verify-object-actual ((obj csf-enum) context when)

  (verify-id obj context)
  (dolist (x (csf-enum.values obj))
    (verify-object-actual x context when))
  (dolist (x (csf-enum.location obj))
    (verify-object-actual x context when))
  t)
  


(defmethod verify-object-actual ((obj csf-typespec) context when)

  (verify-id obj context)
  (dolist (x (csf-typespec.info obj))
    (verify-object-actual x context when))
  (dolist (x (csf-typespec.location obj))
    (verify-object-actual x context when))
  t)


(defmethod verify-object-actual ((obj csf-where) context when)

  (dolist (x (csf-where.location obj))
    (verify-object-actual x context when))
  t)

(defmethod verify-object-actual ((obj csf-info) context when)

  (declare (ignore context when))
  
  (let ((type (csf-info.type obj))
	(value (csf-info.value obj))
	(info (csf-info.info obj)))

    (flet ((%check-type (x name)
	     (unless (or (eq x nil)
			 (and (consp x)
			      (every #'stringp x)))
	       (error "Info-~a for ~s is not nil or list of strings" name obj))))

      (%check-type type "type")
      (%check-type value "value")
      (%check-type info "info")
      )))
    
  

(def-or-method verify-object-actual ((obj (or csf-comment
					      csf-directive
					      csf-location
					      csf-enumval))
				     context when)

  t)

(def-or-method verify-object-actual ((obj (or csf-retval csf-arg)) context when)

  (dolist (x (slot-value obj 'info))
    (verify-object-actual x context when))
;;  (verify-id obj context)
  
  t)

;;(trace verify-object-actual)

