;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: APISPEC -*-

#|

DESC: apispec/api-html.lisp - the code for generating html of the API
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :sds-api-apispec)

(defmethod output-declarations ((obj apispec-toplevel) (lang api-lang-html) stream some-name)
  (declare (ignore some-name))
  
  (let* ((api-name (string-downcase (car (apispec-toplevel.name obj))))
	 (up-api-name (string-upcase api-name)))
    

    (format stream "<title>~a api</title>~2%" api-name)
    
    (format stream "<body bgcolor=white>~2%")
    (format stream "<h1 align=center>~a API</h1>~2%" up-api-name)

    
    (let ((classes (apispec-toplevel.classes obj)))
      (dolist (c classes)
	(output-declarations c lang stream api-name)))

    
    ))

(defvar *colours* '("blue" "red" "green" "purple" "brown" 
		    "darkgreen" "darkblue" "orange" "pink" "lightgreen" "lightblue"))

(defmethod output-declarations ((obj apispec-class) (lang api-lang-html) stream some-name)
  
  (declare (ignore some-name))

  (let* ((elmname (string-downcase (car (apispec-class.elmname obj))))
	 (name (string-downcase (car (apispec-class.name obj))))
	 (class-desc (car (apispec-class.doc obj)))
	 (vars (apispec-class.vars obj))
	 (varlen (length vars))
	 (attrs (apispec-class.attrs obj))
	 (attrlen (length attrs))
	 (subs (apispec-class.subelems obj))
	 (sublen (length subs))
	 (i-docs (apispec-class.infodoc obj))
	 (i-doclen (length i-docs))
	 (c-tbl (make-hash-table :test #'equal))
	 (c-cnt 0)
	 )


    
    (format stream "<h3>Class: ~a</h3>~%" name)


    (format stream "<table bgcolor=\"#f0f0f0\">~%")

    (when elmname
      (format stream "<tr><td><b>XML elementname:</b></td><td colspan=4> ~a</td></tr>~%"
	    elmname))

    (when class-desc
      (format stream "<tr><td><b>Desc:</b></td><td colspan=4> ~a</td></tr>~%"
	    class-desc))
    
    (when (plusp varlen)
      (format stream "<tr><td><b>Variables</b></td><td>Name:</td><td>Type:</td><td>&nbsp;</td><td>Desc:</td></tr>~%")
    
      (dolist (v vars)

	(let ((col (elt *colours* c-cnt))
	      (varname (car (apispec-var.name v))))
	  
	  (setf (gethash varname c-tbl) col)
	  (incf c-cnt)
	  
	  (format stream "<tr><td>&nbsp;</td><td><font color=~a>~a</font></td> <td>~a</td><td>&nbsp;</td><td>~a</td></tr>"
		  col
		  varname
		  (car (apispec-var.type v))
		  (let ((val (car (apispec-var.doc v))))
		    (if val val "&nbsp;"))
		  ))))
    
    (when (plusp attrlen)
      (format stream "<tr><td><b>Attributes</b></td><td>Name:</td><td>Type:</td><td>Variable:</td><td>Desc:</td></tr>~%")
    
      (dolist (v attrs)
	(let* ((varname (car (apispec-attr.var v)))
	       (varcol (gethash varname c-tbl)))
	(format stream "<tr><td>&nbsp;</td><td>~a</td> <td>~a</td><td><font color=~a>~a</font></td><td>~a</td></tr>"
		(car (apispec-attr.name v))
		(car (apispec-attr.type v))
		varcol
		varname
		(let ((val (car (apispec-attr.doc v))))
		  (if val val "&nbsp;"))
		))))

    (when (plusp sublen)
      (format stream "<tr><td><b>Subelements</b></td><td>Name:</td><td>Type:</td><td>Variable:</td><td>Desc:</td></tr>~%")
    
      (dolist (v subs)
	(let* ((varname (car (apispec-subelem.var v)))
	       (varcol (gethash varname c-tbl)))
	(format stream "<tr><td>&nbsp;</td><td>~a</td> <td>~a</td><td><font color=~a>~a</font></td><td>~a</td></tr>"
		(car (apispec-subelem.name v))
		(car (apispec-subelem.type v))
		varcol
		varname
		(let ((val (car (apispec-subelem.doc v))))
		  (if val val "&nbsp;"))
		))))

    (when (plusp i-doclen)
      (format stream "<tr><td><b>Known Info</b></td><td>Type:</td><td>Value:</td><td>Info:</td><td>Desc:</td></tr>~%")
    
      (dolist (v i-docs)
	(let ((ty (car (apispec-infodoc.type v)))
	      (va (car (apispec-infodoc.value v)))
	      (in (car (apispec-infodoc.info v)))
	      (ex (car (apispec-infodoc.expl v))))
	      
	(format stream "<tr><td>&nbsp;</td><td>~a</td> <td>~a</td><td>~a</font></td><td>~a</td></tr>"
		(if ty ty "&nbsp;")
		(if va va "&nbsp;")
		(if in in "&nbsp;")
		(if ex ex "&nbsp;")
		))))


 
    (format stream "</table>~2%")
    ))


  
(defmethod output-code ((obj apispec-toplevel) (lang api-lang-html) stream some-name)

  ;; hack
  (output-declarations obj lang stream some-name))

