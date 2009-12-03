;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: APISPEC -*-

#|

DESC: apispec/api-generate.lisp - the program which generates/updates APIs
Copyright (c) 1998,1999 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :sds-api-apispec)

(defclass api-language ()
  ((name :initarg :name
	 :accessor lang.name)))

(defclass api-lang-lisp (api-language)
  ())

(defclass api-lang-cpp (api-language)
  ())

(defclass api-lang-java (api-language)
  ())

(defclass api-lang-python (api-language)
  ())

(defclass api-lang-html (api-language)
  ())

(defvar *supported-languages* (make-hash-table :test #'equal))

(defun add-language (name class-sym)
  (setf (gethash (string-upcase name) *supported-languages*) 
	#'(lambda () (make-instance class-sym :name name))))

(defun get-language (name)
  (let ((res (gethash (string-upcase name) *supported-languages*)))
    (unless res
      (warn "Unable to find language ~a" name)
      (return-from get-language nil))
    (funcall res)))

;;; added languages

(add-language "lisp" 'api-lang-lisp)
(add-language "cpp" 'api-lang-cpp)
(add-language "java" 'api-lang-java)
(add-language "python" 'api-lang-python)
(add-language "html" 'api-lang-html)

(defgeneric output-declarations (obj lang stream api-name)
  (:documentation "Outputs declaration to given stream"))

(defmethod output-declarations (obj lang stream api-name)
  (declare (ignore api-name))
  (warn "No sensible (~:@(~a ~a ~a ~a ..~))" 
	"output-declarations"
	(its-name obj)
	(its-name lang)
	(its-name stream)))

(defgeneric output-code (obj lang stream api-name)
  (:documentation "Outputs the 'code' to given stream"))

(defmethod output-code (obj lang stream api-name)
  (declare (ignore api-name))
  (warn "No sensible (~:@(~a ~a ~a ~a ..~))" 
	"output-code"
	(its-name obj)
	(its-name lang)
	(its-name stream)))

(defgeneric get-name-of-constant (lang const))
(defgeneric get-name-of-class (lang api-name obj))
(defgeneric get-the-actual-type (lang key))

(defun generate-api (spec-tree &key lang out-dir out-file decl-file src-file)
  "This function takes a tree as the main argument and a few rather important
keyword-parameters as arguments."

  (let ((lang-class (get-language lang)))
    (unless lang-class
      (warn "unable to find language ~a" lang)
      (return-from generate-api nil))
      
    (let ((*print-circle* nil))
      ;;(warn "Switching on ~a" lang-class)
      (cond
	((string-equal lang "cpp")
	     
	 ;; we're c++
	 ;; squeeze out declarations
	 (with-open-file (str (merge-pathnames (pathname decl-file))
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	   (dolist (some-api spec-tree)
	     (output-declarations some-api lang-class str "")))
	 
	 ;;squeeze out some code
	 (with-open-file (str (merge-pathnames (pathname src-file))
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	   (dolist (some-api spec-tree)
	     (output-code some-api lang-class str ""))))

	((string-equal lang "java")
	 ;; we're java
	 (dolist (some-api spec-tree)
	   (output-code some-api lang-class nil out-dir)))

	((string-equal lang "python")
	 ;; we are the python. resistance is 10 Ohm
	 (dolist (some-api spec-tree)
	       (output-code some-api lang-class nil out-dir)))
		
	((string-equal lang "lisp")
	 ;; we're lisp
	 ;;squeeze out some code
	 (with-open-file (str (merge-pathnames (pathname out-file))
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	   (dolist (some-api spec-tree)
	     (output-code some-api lang-class str ""))))
	
	((string-equal lang "html")
	 ;; we write HTML.. maybe it can be used in HTML-OS
	 (with-open-file (str (merge-pathnames (pathname out-file))
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	   (dolist (some-api spec-tree)
	     (output-code some-api lang-class str ""))))


	(t
	 (warn "Unknown language.. ~a" lang)))

      )))


(defun generate-api-from-file (spec-file &key lang out-dir out-file decl-file src-file)
  "parses the given file and then calls GENERATE-API"

  (let ((api-tree (parse-apispec-file spec-file)))
    
    (unless api-tree
      (return-from generate-api-from-file nil))
    
    (generate-api api-tree
		  :lang lang
		  :out-dir out-dir
		  :out-file out-file
		  :decl-file decl-file
		  :src-file src-file)))
