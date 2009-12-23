;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: spres-impl -*-

#|

DESC: spres/files.lisp - various presentation-functions related to files
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres-impl)

(defun tl-find-out-dir (format)
  "Returns a pathname for the out-dir."
  
  ;;Ugly imperative style.
  ;;(warn ">> Ensure out-dir for ~s -> ~s." format (tl-get-outdir-pref format))
  ;;(warn "Pref is ~s" (albert-setting '("system" "directory")))
  (let ((sys-dir (albert-setting '("system" "directory")))
	(pref-dir (tl-get-outdir-pref format)))

    (when (albert-setting '("albert" "presentation" "to-current-dir"))
      (setf sys-dir cl:*default-pathname-defaults*))
    
    (cond ((pathnamep pref-dir) nil)
	  ((stringp pref-dir)
	   (setf pref-dir (pathname (ensure-dir-name pref-dir))))
	  (t
	   (albert-warn "spres> settings dir for ~s is ~s, not string/pathname." format pref-dir)
	   (return-from tl-find-out-dir nil)))
    
    (cond ((pathnamep sys-dir) nil)
	  ((stringp sys-dir)
	   (setf sys-dir (ensure-dir-name (pathname sys-dir))))
	  (t (setf sys-dir cl:*default-pathname-defaults*)))

    ;;(warn "Returning ~s" (merge-pathnames pref-dir sys-dir))
    
    (merge-pathnames pref-dir sys-dir)))
#||    
    
	   (warn "Pref is ~s" (merge-pathnames (pathname  (albert-setting '("system" "directory")))
  (let ((out-dir (ensure-dir-name (tl-get-outdir-pref format))))
    (pathname (ensure-dir-name out-dir))))
||#

(defun tl-get-outdir-pref (format)
  "Returns the actual outdir-preference in the given output-preference.
Returns the actual pref and not a list. Returns NIL on failure."
  (let ((retval nil))
    
    (when format
      (when (typep format 'spres-format)
	(setf format (format.name format)))
      
      (cond ((stringp format)
	     (when-bind (val (albert-setting (list "albert" format "output-dir")))
	       (cond ((stringp val)
		      (setf retval val))
		     (t
		      (warn "Don't know how to handle outdir value ~s for format ~s" val format)))))
	  (t
	   (warn "Unable to handle format to get outdir: ~s" format))))
    
    (unless retval
      (when-bind (val (albert-setting '("albert" "presentation" "output-dir")))
	(cond ((stringp val)
	       (setf retval val))
	      (t
	       (warn "Don't know how to handle outdir value ~s" val)))))
    
    (unless retval
      (setf retval "dumps/"))

    retval))

(defun permute-fname-and-add (f-obj table)
  "permutes the fname with counter till it is addable
to the given table."
  (let ((key (file-info-to-fname f-obj)))
    (multiple-value-bind (obj found-p)
	(gethash key table)
      (declare (ignore obj))
      (cond (found-p
	     (incf (file-info-counter f-obj))
	     (permute-fname-and-add f-obj table))
	    (t
	     (setf (gethash key table) f-obj))))))


(defun search-for-file-names (obj-list table)
  "goes recursively through the object list and adds any 
filenames to the table"

    (dolist (x obj-list)
      ;; hackish
      (let ((?outdir (cond ((typep x '(or sdoc-package sdoc-module))
			    (pathname (strcat (namestring ?outdir)
					      (make-valid-entity (get-object-name x))
					      "/")))
			   (t ?outdir))))
	
	
	(unless (is-empty? x)
	  
	  (when (should-have-individual-file-p x nil)
	    (let ((wanted-file (get-suggested-file-name x nil)))
	      (when wanted-file
		(let ((key (file-info-to-fname wanted-file)))
		  (multiple-value-bind (obj found-p)
		      (gethash key table)
		    (declare (ignore obj))
		    (cond (found-p
			   (permute-fname-and-add wanted-file table))
			  (t
			   (setf (gethash key table) wanted-file))))))))

	  (when (typep x '(or sdoc-package sdoc-module sdoc-class))
	    (calculate-file-list x table))))))


(defmethod calculate-file-list ((object sdoc-toplevel) table)

  (unless table
    (setq table (make-hash-table :test #'equal)))

  (let ((?outdir ""))
    (search-for-file-names (sdoc-toplevel.content object) table))

  (let ((ret-table (make-hash-table :test #'equal)))
    (loop for v being the hash-values of table
	  do
	  (setf (gethash (file-info-id v) ret-table) v))
    
    ret-table))

(defun register-separate-document (fileinfo)
  "Registers the file-info object so that it will be counted."
  (cond ((typep fileinfo 'file-info)
	 (setf (gethash (file-info-id fileinfo) ?file-table) fileinfo)
	 fileinfo)
	(t
	 (albert-warn "spres> cannot register file-entity: ~s" fileinfo)
	 nil)))

(def-or-method calculate-file-list ((object (or sdoc-package sdoc-class sdoc-module)) table)

  (let ((content-list (slot-value object 'content)))
  
    (search-for-file-names content-list table)

    #||
    (when (typep object 'sdoc-package)

      (when (find-if #'(lambda (x) (typep x 'sdoc-class)) content-list)
	(let ((id (strcat "package_" (get-object-name object) "_classlist")))
	  (setf (gethash id table) (make-file-info :id id
						   :dir ?outdir 
						   :fname (make-valid-entity id)
						   :counter 0))))
      
      (when (find-if #'is-generic-fun? content-list)
	(let ((id (strcat "package_" (get-object-name object) "_genfunlist")))
	  (setf (gethash id table) (make-file-info :id id
						   :dir ?outdir 
						   :fname (make-valid-entity id)
						   :counter 0)))))
    ||#
    ))
  
  

(defmethod get-suggested-file-name ((object sdoc-package) context)
  (declare (ignore context))
  (make-file-info :id (get-object-id object)
		  :dir ?outdir
		  :fname "_package"
		  :counter 0))

(defmethod get-suggested-file-name ((object sdoc-module) context)
  (declare (ignore context))
  (make-file-info :id (get-object-id object)
		  :dir ?outdir
		  :fname "_module"
		  :counter 0))


(defmethod get-suggested-file-name ((object sdoc-class) context)
  (declare (ignore context))
  (make-file-info :id (get-object-id object) 
		  :dir ?outdir 
		  :fname (make-valid-entity (get-object-name object))
		  :counter 0))

(defmethod get-suggested-file-name ((object sdoc-method) context)
  (declare (ignore context))
  (make-file-info :id (get-object-id object) 
		  :dir ?outdir 
		  :fname (make-valid-entity (get-object-name object))
		  :counter 0))



(defun file-info-to-fname (f-obj)
  "Translates the file-info obj into a filename string"
  (let ((fname (file-info-fname f-obj))
	(counter (file-info-counter f-obj))
	(dir (file-info-dir f-obj)))
    (unless (non-negative-integer? counter)
      (setf counter 0))
    (when (> counter 0)
      (setf fname (format nil "~a-~a" fname counter)))
    (cond ((pathnamep dir)
	   (setf dir (namestring dir)))
	  ((eq dir nil)
	   (setf dir ""))
	  ((stringp dir) nil)
	  (t
	   (albert-warn "spres> directory ~s for filename ~s is odd, ignored." dir fname)
	   (setf dir "")))
    
    (strcat dir fname)))

(defun tl-ensure-file-dirs (file-table base-dir)
  "given a file-table and a pathname base-dir.  Calls
sds-global:make-sure-dirs-exist."
      ;; ensure directories in place
    (let ((done-dirs nil))
      (loop for val being the hash-values of file-table
	    for dir = (file-info-dir val)
	    do
	    (unless (find dir done-dirs :test #'equal)
;;	      (warn "Merging ~s and ~s -> ~s,~s" ?outdir dir
;;		    (merge-pathnames dir ?outdir)
;;		    (merge-pathnames ?outdir dir)
;;		    )
	      (make-sure-dirs-exist
	       (tl-merge-two-paths dir base-dir))
	      (push dir done-dirs)))))


(defun include-file-entity (f-obj)
  "Returns a string which is a legal file-inclusion
in xml for the given f-obj"
  
  (if (stringp f-obj)
      (strcat "&file" +id-word-delim+ (make-valid-entity f-obj) ";")
      (strcat "&file" +id-word-delim+ (make-valid-entity (file-info-to-fname f-obj)) ";")))
  


(def-or-method should-have-individual-file-p ((object (or sdoc-package
							  sdoc-class
							  sdoc-module)) context)

  ;; to avoid silly warning.. any compiler should wipe it
  (when (and nil object context))
    
  t)

(defmethod should-have-individual-file-p ((object sdoc-method) context)

  (when (is-generic-fun? object)
    (when (>= (length (sdoc-method.content object))
	      (setting-or-default '("albert" "presentation" "gf" "separatepage") 2))
      ;;(warn "File for ~s" object)
      t)))


(defun tl-make-new-obj-document (obj &optional old-doc)
  "presents an independent file.."
  (let ((f-obj (gethash (get-object-id obj) ?file-table)))

    ;;(warn "Filename ~a -> [~a] ~a" f-obj ?outdir (file-info-to-fname f-obj))

    ;; insert into the old-document an insert
    (when old-doc
      (put old-doc (include-file-entity f-obj) (eol)))
    
    ;; here is a directory-making bug
    (make-document ?outdir
		   (file-info-to-fname f-obj) 
		   ?format
		   ?language)))

(defun tl-make-new-document (filename old-doc)
  
  (when old-doc
    (put old-doc (include-file-entity filename) (eol)))

  (make-document ?outdir
		 (if (stringp filename)
		     (make-valid-entity filename :allow '(#\/))
		     (file-info-to-fname filename))
		 ?format
		 ?language))


(defun tl-get-ok-obj-document (obj old-doc)
  "Returns a new document if the OBJ requires an
idividual file, otherwise returns the given document."
  
;;  (warn "Getting document from ~a ~a -> ~a" obj old-doc
;;	(should-have-individual-file-p obj nil))

  (if (should-have-individual-file-p obj nil)
      (tl-make-new-obj-document obj old-doc)

      old-doc))


(defun tl-possible-close-document (obj doc)
  "If the object OBJ requires it's own file, close this file,
assuming it was opened by WITH-OK-DOCUMENT and therefore is
safe.  The closed document is also presented."
  
  (when (should-have-individual-file-p obj nil)
;;    (warn "closing/presenting ~a document ~a" obj doc)
    (present-document doc)))

(defmacro with-ok-obj-document (the-info &body the-body)
  "Makes a subdocument I think and encloses creation of the document
for the given object and closing of the document."
  (unless (and (consp the-info) (= 3 (length the-info)))
    (error "WITH-OK-DOCUMENT 1st argument should be (DOC-VAR OBJ OLD-DOC)"))
  
  (let ((gs (gensym))
	(old-doc (third the-info))
	(the-obj (second the-info))
	(doc-name (first the-info)))
    
    `(let ((,gs ,old-doc))
      (unwind-protect
	   (let ((,doc-name (tl-get-ok-obj-document ,the-obj ,old-doc)))
	     (unwind-protect
		  ,@the-body
	       (tl-possible-close-document ,the-obj ,doc-name)))
	(setf ,old-doc ,gs)))
    ))

(defmacro with-ok-document (the-info &body the-body)
  "Makes a document and presents it I think, it's not connected to a specific obj."
;;  (unless (and (consp the-info) (= 3 (length the-info)))
;;    (error "WITH-OK-DOCUMENT 1st argument should be (DOC-VAR OBJ OLD-DOC)"))
  
  (let ((gs (gensym))
	(old-doc (third the-info))
	(fname (second the-info))
	(doc-name (first the-info))
	)
    
    `(let ((,gs ,old-doc))
      (unwind-protect
	   (let ((,doc-name (tl-make-new-document ,fname ,old-doc)))
	     (unwind-protect
		  ,@the-body
	       (present-document ,doc-name)))
	(setf ,old-doc ,gs)))
    ))
