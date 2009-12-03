;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: APISPEC-XML -*-

#|

DESC: apispec/xml-sexp.lisp - reads xml in sexp form
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :apispec-xml)

(defvar *just-read-objs* nil)

;;(declaim (type simple-vector *loc-sym-table*))

(defun parse-sexp-obj (obj xml-tool)
;;  (print obj)
;;  (print (car obj))
;;  (print (cadr obj))
;;  (declare (optimize (safety 0) (speed 3) (debug 0)))

  (let ((name (symbol-name (svref *loc-sym-table* (car obj)))))
    (declare (type simple-base-string name))
      
    (parse-element-start xml-tool 
			 name
			 (cadr obj))
    
    (let ((rest (cddr obj)))
      (when rest
	(if (stringp (car rest))
	    (parse-element-content xml-tool (car rest))
	  (dolist (i rest)
	    (parse-sexp-obj i xml-tool)))))
    (parse-element-end xml-tool name)))


(defun read-sx-file (fname xml-tool)
  (let ((*just-read-objs* nil)
	(*loc-sym-table* nil))
    (load fname)
    (when-verbose
	(albert-info "sexp> done loading of ~a" fname))
    (dolist (i *just-read-objs*)
      (parse-sexp-obj i xml-tool)))
  t)






#||
(defun parse-all (xml-tool)
  (dolist (i *just-read-objs*)
    (parse-obj i xml-tool)))

(defun tsx ()
  (let ((*just-read-objs* nil)
	(the-tool (make-xml-tool (sds-api-csf:make-csf-factory))))
    (read-file "../expat/foo.sx")
    (parse-all the-tool)
    
    (with-open-file (str (pathname "dumpy.testy")
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
        
        (format str "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>~%")
        (format str "<!DOCTYPE sdoc SYSTEM \"sdoc.dtd\">~%")
        
        (sds-xml:print-as-xml (car (xml-tool.top-objects the-tool))
			      str the-tool))))


||#

