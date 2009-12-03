;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: albert -*-

#|

DESC: tools/sds-asdf.lisp - tools for working with sysdefs
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :albert)

(defun mapappend (function list)
  (let ((f (coerce function 'function)))
    (loop for i in list append (funcall f i))))

(defun get-late-bind-sym (package name)
  "Tries to find a sym that may not exist at read, compile
or load time, ie totally dynamic."
  (when-bind (pack (find-package package))
    (find-symbol (symbol-name name) pack)))

(defun get-late-bind-function (package name)
  "Tries to find a function that may not exist at read, compile
or load time, ie totally dynamic."
  (let* ((pack (find-package package))
         (sym (find-symbol (symbol-name name) pack)))
    (when (fboundp sym)
      (fdefinition sym))))

(defmacro glbf (pack name &rest args)
  (let ((res (gensym "fun-name")))
    `(when-bind (,res (get-late-bind-function ,pack ,name))
      (funcall ,res ,@args))))

(defun %get-default-directory ()
  "The default directory."
  #+allegro (excl:current-directory)
  #+clisp (ext:default-directory)
  #+cmu (ext:default-directory)
;;  #+sbcl (sb-ext:default-directory)
  #+cormanlisp (ccl:get-current-directory)
  #+lispworks (hcl:get-working-directory)
  #+lucid (lcl:working-directory)
  #+sbcl (truename ".")
  #-(or allegro sbcl clisp cmu cormanlisp lispworks lucid) (truename "."))

(defun is-asdf-object? (obj)
  (typep obj (get-late-bind-sym :asdf :component)))

(defun is-mkdefsys-object? (obj)
  (typep obj (get-late-bind-sym :make :component)))

(defun find-system-object (name)
  (let ((retval nil))

    (ignore-errors
      (when-bind (find-sys (get-late-bind-function :asdf :find-system))
	(setf retval (funcall find-sys name))))

    (ignore-errors
      (unless retval
	(when-bind (find-sys (get-late-bind-function :make :find-system))
	  (setf retval (funcall find-sys name)))))
      
    retval))
   

(defun get-system-pathname (system)
  "Returns the path to the system-def."

  (when (eq system nil)
    (return-from get-system-pathname nil))

  (cond ((or (stringp system) (symbolp system))
	 (get-system-pathname (find-system-object system)))
	
	((is-asdf-object? system)
	 (when-bind (cp (get-late-bind-function :asdf :component-pathname))
	   (funcall cp system)))
	
	((is-mkdefsys-object? system)
	   (when-bind (cp (get-late-bind-function :make :component-root-dir))
	     (funcall cp system)))
	
	(t nil)))

(defun all-asdf-components (component)
  (cond ((typep component (get-late-bind-sym :asdf :cl-source-file))
	 (list component))
	((typep component (get-late-bind-sym :asdf :source-file))
	 nil)
	((typep component (get-late-bind-sym :asdf :module))
	 (when-bind (mc (get-late-bind-function :asdf :module-components))
	   (cons component (mapappend #'all-asdf-components
				   (funcall mc component)))))
	((or (stringp component) (symbolp component))
	 (when-bind (find-sys (get-late-bind-function :asdf :find-system))
	   (all-asdf-components (funcall find-sys component))))
	(t nil)))
	
	 

(defun get-system-files-asdf (system)
  "Returns a list of files for the system"
  (let* ((system (glbf :asdf :find-system system))
	 (sys-path  (glbf :asdf :component-pathname system))
	 (base-path (make-pathname
		     :directory (butlast (pathname-directory sys-path))
		     :defaults sys-path))
	 (comp-files (mapcar (get-late-bind-function :asdf :component-pathname)
			     (all-asdf-components system)))
	 (cur-dir (%get-default-directory))
	 (files (mapcar #'(lambda (x) (enough-namestring x cur-dir)) comp-files)))
    (remove-if-not #'pathname-name files)))

   

(defun all-mkdefsys-components (component)

  (cond ((typep component (get-late-bind-sym :make :component))
	 (when-bind (comp-list (get-late-bind-function :make :component-components))
	   (let ((components (funcall comp-list component)))
	     (cond (components 
		    (mapappend #'all-mkdefsys-components components))
		   (t (list component))))))
	((or (typep component 'symbol)
	     (stringp component))
	 (when-bind (find-sys (get-late-bind-function :make :find-system))
	   (all-mkdefsys-components (funcall find-sys component))))
	(t
	 (error "Don't know how to handle ~s" component))
	))



(defun get-system-files-mkdefsys (system)
  "Returns a list of files for the system"
  (when-bind (full-path (get-late-bind-function :make :component-full-pathname))
    (mapcar #'(lambda (c) (funcall full-path c :source))
	    (all-mkdefsys-components system))))



(defun register-albert-settings-asdf (system)
  "Checks a system for info and registers it as albert-settings."

  (unless system
    (error "No system provided for registering settings."))
    
  (unless (typep system (get-late-bind-sym :asdf :system))
    (setf system (glbf :asdf :find-system system)))

  (unless (typep system (get-late-bind-sym :asdf :system))
    (error "Unable to find asdf:system specified as: ~s" system))
  
  ;;(warn "&&& Registering stuff from ~s" system)

  (ignore-errors
    (when-bind (name (glbf :asdf :component-name system))
      (setf (albert-setting '("system" "name")) name)))

  (ignore-errors
    (when-bind (path (get-system-pathname system))
      (setf (albert-setting '("system" "directory")) path)))
  
  (ignore-errors
    (when-bind (author (glbf :asdf :system-author system))
      (setf (albert-setting '("system" "author" "name")) author)))

  (ignore-errors
    (when-bind (maint (glbf :asdf :system-maintainer system))
      (setf (albert-setting '("system" "maintainer" "name")) maint)))

  (ignore-errors
    (when-bind (author (glbf :asdf :system-licence system))
      (setf (albert-setting '("system" "licence" "name")) author)))

  (ignore-errors
    (when-bind (version (glbf :asdf :component-version system))
      (setf (albert-setting '("system" "version" "number")) version)))

  (ignore-errors
    (when-bind (desc (glbf :asdf :system-description system))
      (setf (albert-setting '("system" "description")) desc)))

  (flet ((to-string (arg)
	   (typecase arg
	     (string arg)
	     (symbol (string-downcase (string arg))))))
	     
  (ignore-errors
    ;; assumes it's an alist
    (let ((props (glbf :asdf :component-properties system)))
      (dolist (i props)
	
	(cond ((consp i)

	       (cond ((consp (car i)) ;; some are lists

		      (cond ((equal (car i) '("system" "author" "email"))
			     (setf (albert-setting (car i)) (cdr i)))
			    ((equal (car i) '("system" "date"))
			     (setf (albert-setting (car i)) (cdr i)))
			    ((equal (car i) '("system" "directory"))
			     (setf (albert-setting (car i)) (cdr i)))
			    ((equalp (mapcar #'to-string (car i)) '("albert" "formats"))
			     (setf (albert-setting '("albert" "presentation" "formats")) (cdr i)))
			    ((equalp (mapcar #'to-string (car i)) '("albert" "output-dir"))
			     (setf (albert-setting '("albert" "presentation" "output-dir")) (cdr i)))
			    ((equalp (mapcar #'to-string (car i)) '("albert" "docbook" "template"))
			     (setf (albert-setting '("albert" "docbook" "generate")) (cdr i)))
			    ((string-equal (string (first (car i))) "albert") ;; if albert is first in the list
			     (setf (albert-setting (mapcar #'to-string (car i))) (cdr i)))
			    (t
			     nil)))

		     ((or (symbolp (car i)) (stringp (car i)))
		      (cond ((string-equal (string (car i)) "author-email")
			     (setf (albert-setting '("system" "author" "email")) (cdr i)))
			    ((string-equal (string (car i)) "date")
			     (setf (albert-setting '("system" "date")) (cdr i)))
			    ((string-equal (string (car i)) "licencefile")
			     (setf (albert-setting '("system" "licencefile")) (cdr i)))
			    (t
			     (warn "Albert-ASDF: Checking, but not handling: ~s" (car i)))))

		     
		     ))
	      (t
	       (warn "Odd format for property: ~s" i)))
	)))

  t))

(defun register-albert-settings (system)
  "Checks a system for info and registers it as albert-settings."
  (cond ((or (stringp system) (symbolp system))
	 (register-albert-settings (find-system-object system)))
	((is-asdf-object? system)
	 (register-albert-settings-asdf system))
	((eq system nil) nil)
	(t
	 (warn "Only ASDF supported for REGISTER-ALBERT-SETTINGS yet, not ~s." system))))


  
(defun get-system-files (system)
  "Returns a list of files for the system"
  (let ((retval nil)
	(sys (find-system-object system)))

    (cond ((not sys)
	   (warn "Unable to find system ~s" system))
	  (t
	   (cond ((is-asdf-object? sys)
		  (setf retval (get-system-files-asdf sys)))
		 ((is-mkdefsys-object? sys)
		  (setf retval (get-system-files-mkdefsys system)))
		 (t
		  ;; (warn "Only ASDF and MKDEFSYS supported for GET-SYSTEM-FILES yet.")
		  (warn "Unable to handle system-object ~s" sys))
		 )))
    retval))


(defun show-files (system)
  (let ((files (get-system-files system)))
    (dolist (i files)
      (format t "~&~s~%" i))))

(defun document-files (files &key (use-clean-readtable t) (display-progress nil))
  "Documents the list of FILES."

  (unless files
    (warn "No files submitted to DOCUMENT-FILES")
    (return-from document-files nil))
  
  (let ((retval nil)
	(apispec-base:*verbose-operation* (albert-setting '("albert" "verbose")))
	(apispec-base:*submarine-quiet* (albert-setting '("albert" "submarine-quiet")))
	(spres-impl:*rule-trace* (albert-setting '("albert" "presentation" "trace-rules")))
	(spres-impl:*clhs-root* (albert-setting '("hyperspec" "root")))
	(cl:*load-verbose* (albert-setting '("albert" "verbose")))
	(temporary-file nil)
	(csf-tree nil)
	(sdoc-tree nil)
	)

    (when-verbose
	(albert-info "Will now analyse files in given order:~%   ~s" files))

    (when (albert-setting '("albert" "use-temporary-files"))
      (setf temporary-file (albert-setting '("albert" "lisp2csf" "outfile"))))
    
    (when (albert-setting '("albert" "lisp2csf" "display-progress"))
      (setf display-progress t))
    
    (setf csf-tree (lisp2csf:analyse-files files :out-file temporary-file
					   :display-progress display-progress
					   :use-clean-readtable use-clean-readtable))
    
    (unless (consp csf-tree)
      (warn "LISP2CSF seemed to fail (no returned value), exiting")
      (return-from document-files nil))
    
    ;;(warn "RETVAL is ~s" csf-tree)
    
    (when (albert-setting '("albert" "use-temporary-files"))
      (setf csf-tree (csf:parse-csf-file temporary-file)) ;; last file
      (unless (consp csf-tree)
	(warn "Unable to load temporary CSF-file: ~s" temporary-file)
	(return-from document-files nil)))

    (setf temporary-file nil)
    
    (when (albert-setting '("albert" "use-temporary-files"))
      (setf temporary-file (albert-setting '("albert" "csf2sdoc" "outfile"))))
    
    
    (setf sdoc-tree (albert:convert-csf-to-sdoc csf-tree :out-file temporary-file ))

    (unless (consp sdoc-tree)
      (warn "CSF2SDOC seemed to fail (no returned value), exiting")
      (return-from document-files nil))
    
    ;;(warn "RETVAL is ~s" sdoc-tree)

    (when (albert-setting '("albert" "use-temporary-files"))
      (setf sdoc-tree (sdoc:parse-sdoc-file temporary-file)) ;; last file
      (unless (consp sdoc-tree)
	(warn "Unable to load temporary SDOC-file: ~s" temporary-file)
	(return-from document-files nil)))
    
    
    (setf retval (spres:present-sdoc sdoc-tree))
    
    ;;(warn "RETVAL is ~s" retval)
    
    retval))

(defun document-systems (&rest systems)
  "Produces a DocBook XML file which contains markup that represents
packages, methods, classes, functions, etc. extracted from 'systems'."
  (mapc #'register-albert-settings (reverse systems))
  (document-files (flatten (mapcar #'(lambda (system)
                                       (get-system-files system)) systems))))
