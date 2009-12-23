;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: albert -*-

#|

DESC: tools/base.lisp - declarations
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :albert)

(defgeneric add-documentation (obj text)
  (:documentation "Adds docs to a given object."))

(defgeneric convert-obj (obj result-type)
  (:documentation
   "Converts the object obj to the given result-type"))

(defgeneric find-matching-container (obj table)
  (:documentation "WRITE DOC LATER"))


(defmethod add-documentation (obj text)
  (declare (ignore text))
  (warn "No ADD-DOCUMENTATION handler written for ~a" obj))

(defmethod convert-obj (obj result-type)
  (error "No conversion has been written from ~a to type ~a"
	 (its-name obj) result-type)
  nil)
