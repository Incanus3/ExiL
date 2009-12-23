;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: albert -*-

#|

DESC: tools/convert.lisp - utilities for converting between objects
Copyright (c) 1998-2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :albert)

(defstruct tab-entry
  scope
  module
  categories)

(defstruct tab-catentry
  scope
  category)

(defstruct tab-docentry
  loc
  text
  closest
  closest-loc)

;;(defvar *doc-table* nil 
;;  "placeholder for csf-comment")

(defvar *module-table* nil 
  "just a place to save stuff during dev.")

(defvar *active-language* :lisp "the language we treat code from.")

(defvar *comment-parsers* (make-hash-table :test #'eq))
(defvar *current-doc-parser* nil)

(defun establish-comment-parser& (key fun)
  "Ensures that a given function with accompanying key is ready for use."
  (setf (gethash key *comment-parsers*) fun))

(defun get-comment-parser (key)
  "Returns a parser for comments from a given key."
  (gethash key *comment-parsers*))


#||
(defun check-for-any-doc (loc obj) 
  (declare (ignore obj))
  (warn "Calling CHECK-FOR-ANY-DOC")
  (let* ((the-file (car (sdoc-location.file loc)))
	 (res (gethash the-file *doc-table*)))
    (when res
      (let ((start-line (car (sdoc-location.startline loc)))
	    (end-line (car (sdoc-location.endline loc))))
	(format t "Searching in ~a for line ~a ~a~%" the-file start-line end-line)))
    ))
||#

(defun parse-javadoc-text (text)
  "parses a doc-string and returns the content as a list
of conses (TAG . CONTENT) where both are text. When TAG is 
NIL there was no tag (ie. plain text).  Using @ to mark a
tag in text."

  (declare (type simple-base-string text))

;;  (when-verbose
;;      (warn "Textlen is ~a~%" text))
  
  
  (when (= (length text) 0)
    (return-from parse-javadoc-text nil))

  ;; first we skip leading whitespace
  (let* ((parse-text (string-left-trim '(#\Space #\Tab #\Newline) text))
	 (textlen (the fixnum (- (the fixnum (length parse-text)) 1)))
	 (res nil)
	 (last-start 0)
	 (cur-kwd nil)
	 (last-char #\Space))

    (declare (type simple-base-string parse-text))
    (declare (type character last-char))
    (declare (type fixnum last-start textlen))

;;    (when (find #\@ parse-text :test #'equal)
;;      (warn "Checking out ~s" parse-text))

    
    (loop for i of-type fixnum from 0 to textlen
	  for c of-type character = (schar parse-text i)
	  do
	  (cond ((eq c #\@)
;;		 (format t "(at) found~%")
		 (cond ((and
			 (or (eql last-char #\Newline)
			     (eql last-char #\Space)
			     (eql last-char #\Tab))
			 (not (or (digit-char-p (schar parse-text (1+ i)))
				  (eql (schar parse-text (1+ i)) #\Space)
				  (eql (schar parse-text (1+ i)) #\Tab)
				  (eql (schar parse-text (1+ i)) #\NewLine))))

;;			(format t "Found @: ~c~c~c~c~%" last-char (schar parse-text i)
;;				(schar parse-text (1+ i))
			;;				(schar parse-text (+ i 2)))
			
			;; are we right at start
			(unless (eq i 0)
			  
			  (push (cons cur-kwd (subseq parse-text last-start
						      (the fixnum (- i 1))))
				res)
			  (setq cur-kwd nil))

			;; parse kwd
			(loop for j of-type fixnum from (the fixnum (1+ i))
			      for ch of-type character = (schar parse-text j)
			      until
			      
			      ;; when we have come to end of word
			      (when (or (eq ch #\Newline)
					(eq ch #\Space)
					(eq ch #\Tab))
				(setq cur-kwd (subseq parse-text (the fixnum (1+ i)) j))
				;; (format t "kwd: ~a~%" cur-kwd)
				(setq i j)
				(setq last-start j)
				(setq last-char #\@)
				(return nil))))
		       (t
			(setq last-char c))))
		(t
		 (setq last-char c))))

    ;; add remaining text
    (push (cons cur-kwd (subseq parse-text last-start))
	  res)

    ;; return the list
    (nreverse res)))	 

(defun %make-doc-obj (type val)
  "This basically creates a SDOC-DOC and assigns values to fields"
  
  (let ((obj (make-sdoc-doc)))
    (setf (sdoc-doc.type obj) (list type))
    (setf (sdoc-doc.text obj) (list val))
    obj))

(defun %make-info-obj (type val info)
  "This basically creates a SDOC-INFO and assigns values to fields"
  (let ((obj (make-sdoc-info)))
    ;;    (warn "making info ~a ~a ~a" type val info)
    (when type
      (setf (sdoc-info.type obj) (list type)))
    (when val
      (setf (sdoc-info.value obj) (list val)))
    (when info
      (setf (sdoc-info.info obj) (list info)))
    obj))


(def-or-method add-documentation ((obj (or sdoc-variable 
					   sdoc-typespec
					   sdoc-enum
					   sdoc-class
					   sdoc-method))
				  text)
  (warn "doc-parse for ~s ~s" (class-name (class-of obj)) text)
  
  (let ((doc-list (funcall *current-doc-parser* text)))
    
    (dolist (x doc-list)
      (cond 
	((eq (car x) nil)
	 (push (%make-doc-obj "desc" (cdr x)) (slot-value obj 'doc)))
	;; we just say stuff are allowed.. no need to bitch here (yet)
	(t
	 (push (%make-doc-obj (car x) (cdr x)) (slot-value obj 'doc)))))))



;;; experiment
(defmethod convert-obj ((loc csf-location) (new-loc sdoc-location))

  (setf (sdoc-location.file new-loc) (csf-location.file loc))
  (setf (sdoc-location.startline new-loc) (csf-location.startline loc))
  (setf (sdoc-location.startcol new-loc) (csf-location.startcol loc))
  (setf (sdoc-location.endline new-loc) (csf-location.endline loc))
  (setf (sdoc-location.endcol new-loc) (csf-location.endcol loc))
  
  new-loc)


(defmethod convert-obj ((axx csf-access) (new-axx sdoc-access))
  
  (setf (sdoc-access.visibility new-axx) (csf-access.visibility axx))
  (setf (sdoc-access.scope new-axx) (csf-access.scope axx))
  
  new-axx)
  

(defmethod convert-obj ((old-enumval csf-enumval) (new-enumval sdoc-enumval))
  
  (setf (sdoc-enumval.name  new-enumval) (csf-enumval.name old-enumval))
  (setf (sdoc-enumval.value new-enumval) (csf-enumval.value old-enumval))
  
  new-enumval)


(defmethod convert-obj ((old csf-inherit) (new sdoc-inherit))
  
  (setf (sdoc-inherit.name new)     (csf-inherit.name old))
  
  (convert-list (csf-inherit.info old) 
		(sdoc-inherit.info new) 
		make-sdoc-info)
  
  new)

(defmethod convert-obj ((old csf-retval) (new sdoc-retval))
  (convert-list (csf-retval.info old) 
		(sdoc-retval.info new) 
		make-sdoc-info)
  new)

(defmethod convert-obj ((old csf-arg) (new sdoc-arg))
  (convert-list (csf-arg.info old) 
		(sdoc-arg.info new) 
		make-sdoc-info)
  new)


(defmethod convert-obj ((old csf-info) (new sdoc-info))

  (setf (sdoc-info.type new)  (csf-info.type old))
  (setf (sdoc-info.value new) (csf-info.value old))
  (setf (sdoc-info.info new)  (csf-info.info old))
  
  new)


(defmethod convert-obj ((old modspec-doc) (new sdoc-doc))

  (setf (sdoc-doc.type new) (modspec-doc.type old))
  (setf (sdoc-doc.text new) (modspec-doc.text old))
  
  new)


(defmethod convert-obj ((old csf-where) (new sdoc-where))

  (setf (sdoc-where.what new)  (csf-where.what old))
  
  (convert-list (csf-where.location old) 
		(sdoc-where.location new) 
		make-sdoc-location)

  new)


(defmethod convert-obj ((old csf-class) (new-cl sdoc-class))

  (setf (sdoc-class.id new-cl) (csf-class.id old))
  (setf (sdoc-class.name new-cl) (csf-class.name old))

  (convert-list (csf-class.info old) 
		(sdoc-class.info new-cl) 
		make-sdoc-info)
  
  (convert-list (csf-class.location old) 
		(sdoc-class.location new-cl) 
		make-sdoc-location)
  
  ;;    (check-for-any-doc (car (sdoc-classinfo.location info)) info)
  
  (convert-list (csf-class.access old) 
		(sdoc-class.access new-cl)
		make-sdoc-access)
  
  (convert-list (csf-class.parents old) 
		(sdoc-class.parents new-cl) 
		make-sdoc-inherit)

  (flet ((%check-info-obj (x)
;;	   (warn "parser is ~a" *current-doc-parser*)
	   (if (string-equal "documentation"
			     (car (sdoc-info.type x)))
	       (let ((doc-objs (funcall *current-doc-parser*
					(car (sdoc-info.value x)))))
		 (dolist (i doc-objs)
		   (cond 
		     ((eq (car i) nil)
		      (push (%make-doc-obj "desc" (cdr i))
			    (slot-value new-cl 'doc)))
		     (t
		      (push (%make-doc-obj (car i) (cdr i))
			    (slot-value new-cl 'doc)))))
		 nil)
	       x)))
    
    (let* ((info-list (sdoc-class.info new-cl))
	   (new-list (flatten (mapcar #'%check-info-obj info-list))))

      (setf (sdoc-class.info new-cl) new-list)))
    
  
  (setf (sdoc-class.content new-cl) 
	(convert-csf-list (csf-class.content old)))
  
  new-cl)


(defmethod convert-obj ((old csf-package) (new-cl sdoc-package))

  (setf (sdoc-package.id new-cl) (csf-package.id old))
  (setf (sdoc-package.name new-cl) (csf-package.name old))

  (convert-list (csf-package.info old) 
		(sdoc-package.info new-cl) 
		make-sdoc-info)
  
  (convert-list (csf-package.location old) 
		(sdoc-package.location new-cl) 
		make-sdoc-location)
  
  ;;    (check-for-any-doc (car (sdoc-classinfo.location info)) info)
  
  (setf (sdoc-package.content new-cl) 
	(convert-csf-list (csf-package.content old)))

  ;; hack
  (let ((the-list (sdoc-package.content new-cl))
	(new-list '())
	(add nil))
    (dolist (i the-list)
      (setf add i)
      (when (typep i 'sdoc-method)
	(when (is-method? i)
	  (when-bind (gf (find-generic-parent the-list i))
	    (push i (sdoc-method.content gf))
	    (setf add nil)
	    ;;(warn "Matching gf for ~s is ~s" i gf)
	    )))
      (when add
	(push add new-list)))

    (setf (sdoc-package.content new-cl) new-list))
  
  new-cl)


(defun find-generic-parent (obj-list obj)
  (dolist (i obj-list)
    (when (is-generic-fun? i)
      (when (equal (get-object-name obj) (get-object-name i))
	(return-from find-generic-parent i)))))



(defmethod convert-obj ((old csf-typespec) (new-t sdoc-typespec))

  (setf (sdoc-typespec.id new-t) (csf-typespec.id old))
  (setf (sdoc-typespec.name new-t) (csf-typespec.name old))
  
  (convert-list (csf-typespec.location old) 
		(sdoc-typespec.location new-t) 
		make-sdoc-location)
  
  (convert-list (csf-typespec.access old) 
		(sdoc-typespec.access new-t) 
		make-sdoc-access)

  (convert-list (csf-typespec.info old) 
		(sdoc-typespec.info new-t) 
		make-sdoc-info)
    
  new-t)


(defmethod convert-obj ((old csf-directive) (new sdoc-directive))

  (setf (sdoc-directive.name new) (csf-directive.name old))
  (setf (sdoc-directive.value new) (csf-directive.value old))
  (setf (sdoc-directive.info new) (csf-directive.info old))
  
  (convert-list (csf-directive.location old) 
		(sdoc-directive.location new) 
		make-sdoc-location)
  
  new)


(defmethod convert-obj ((old csf-enum) (new-t sdoc-enum))

  (setf (sdoc-enum.id new-t) (csf-enum.id old))
  (setf (sdoc-enum.name new-t) (csf-enum.name old))
    
  (convert-list (csf-enum.values old) 
		(sdoc-enum.values new-t) 
		make-sdoc-enumval)
  
  (convert-list (csf-enum.location old) 
		(sdoc-enum.location new-t) 
		make-sdoc-location)
  
  (convert-list (csf-enum.access old) 
		(sdoc-enum.access new-t) 
		make-sdoc-access)
    
  new-t)


(defmethod convert-obj ((old csf-variable) (new sdoc-variable))
    
  (setf (sdoc-variable.name new) (csf-variable.name old))
  (setf (sdoc-variable.id   new) (csf-variable.id old))

  (convert-list (csf-variable.info old) 
		(sdoc-variable.info new) 
		make-sdoc-info)

  (convert-list (csf-variable.location old) 
		(sdoc-variable.location new) 
		make-sdoc-location)
    
  (convert-list (csf-variable.access old) 
		(sdoc-variable.access new) 
		make-sdoc-access)

  
  (flet ((%check-info-obj (x)
;;	   (warn "parser is ~a" *current-doc-parser*)
	   (if (string-equal "documentation"
			     (car (sdoc-info.type x)))
	       (let ((doc-objs (funcall *current-doc-parser*
					(car (sdoc-info.value x)))))
		 (dolist (i doc-objs)
		   (cond 
		     ((eq (car i) nil)
		      (push (%make-doc-obj "desc" (cdr i))
			    (slot-value new 'doc)))
		     (t
		      (push (%make-doc-obj (car i) (cdr i))
			    (slot-value new 'doc)))))
		 nil)
	       x)))
    
    (let* ((info-list (sdoc-variable.info new))
	   (new-list (flatten (mapcar #'%check-info-obj info-list))))
      
      (setf (sdoc-variable.info new) new-list)))
    
  
  new)

(defmethod convert-obj ((meth csf-method) (sd-meth sdoc-method))

  (setf (sdoc-method.id sd-meth) (csf-method.id meth))
  (setf (sdoc-method.name sd-meth) (csf-method.name meth))

  (convert-list (csf-method.info meth) 
		(sdoc-method.info sd-meth) 
		make-sdoc-info)
  
  (convert-list (csf-method.where meth) 
		(sdoc-method.where sd-meth) 
		make-sdoc-where)
  
  (convert-list (csf-method.access meth) 
		(sdoc-method.access sd-meth) 
		make-sdoc-access)
  
  (convert-list (csf-method.retvals meth) 
		(sdoc-method.retvals sd-meth) 
		make-sdoc-retval)
  
  (convert-list (csf-method.args meth) 
		(sdoc-method.args sd-meth)
		make-sdoc-arg)

  ;; hack to fix documentation info into doc

  (flet ((%check-info-obj (x)
;;	   (warn "parser is ~a" *current-doc-parser*)
	   (if (string-equal "documentation"
			     (car (sdoc-info.type x)))
	       (let ((doc-objs (funcall *current-doc-parser*
					(car (sdoc-info.value x)))))
		 (dolist (i doc-objs)
		   (cond 
		     ((eq (car i) nil)
		      (push (%make-doc-obj "desc" (cdr i))
			    (slot-value sd-meth 'doc)))
		     (t
		      (push (%make-doc-obj (car i) (cdr i))
			    (slot-value sd-meth 'doc)))))
		 nil)
	       x)))
  
    (let* ((info-list (sdoc-method.info sd-meth))
	   (new-list (flatten (mapcar #'%check-info-obj info-list))))
      
      (setf (sdoc-method.info sd-meth) new-list)))
    
  
  sd-meth)
    
(defmethod convert-obj ((cat modspec-category) (new-cat sdoc-category))

  ;;  (when-verbose
  ;;    (format t "Converting a modspec-cat to a sdoc-cat~%"))  
  
  (setf (sdoc-category.name new-cat) (modspec-category.name cat))
  (setf (sdoc-category.type new-cat) (list "userspec"))

  (let ((any-desc (modspec-category.desc cat)))
    (when any-desc
      (push (%make-doc-obj "desc" (car any-desc)) (sdoc-category.doc new-cat))))

  ;; content will be added later..
  new-cat)

(defmethod convert-obj ((mod modspec-module) (module sdoc-module))

  ;;  (when-verbose
  ;;    (format t "Converting a modspec-module to a sdoc-module~%"))
  
  (setf (sdoc-module.name module) (modspec-module.name mod))
  (setf (sdoc-module.fullname module) (modspec-module.fullname mod))
  
  (setf (sdoc-module.id module) (list (strcat "@module@" (car (sdoc-module.name module))
					      "@" (if (sdoc-module.fullname module)
						      (car (sdoc-module.fullname module))
						      "") "@@@")))
  ;;  (warn "id: ~a" (sdoc-module.id module))
  
  ;; some of this should go doc though..
  (dolist (x (modspec-module.info mod))
    (push (%make-info-obj 
	   (car (modspec-info.type x))
	   (car (modspec-info.value x))
	   (car (modspec-info.info x))) 
	  (sdoc-module.info module)))

  (convert-list (modspec-module.categories mod) 
		(sdoc-module.content module) 
		make-sdoc-category)
  
  module)



(defmethod convert-obj ((pck modspec-package) (package sdoc-package))

  ;;  (when-verbose
  ;;    (format t "Converting a modspec-module to a sdoc-module~%"))
  
  (let ((the-name (modspec-package.name pck)))
  
    (setf (sdoc-package.name package) the-name)
    
    (setf (sdoc-package.id package) (list (strcat "@package@" (car the-name)
						  "@" (car the-name) "@@@")))
    ;;  (warn "id: ~a" (sdoc-module.id module))

    (convert-list (modspec-package.doc pck)
		  (sdoc-package.doc package)
		  make-sdoc-doc)
    
#||    
    (let ((any-desc (modspec-package.desc pck)))
      (when any-desc
	(let ((doc-obj (make-sdoc-doc)))
	  (setf (sdoc-doc.type doc-obj) (list "desc"))
	  (setf (sdoc-doc.text doc-obj) any-desc)
	  (push doc-obj (sdoc-package.doc package)))))
||#  
    ;; some of this should go doc though..
    (dolist (x (modspec-package.info pck))
      (push (%make-info-obj 
	     (car (modspec-info.type x))
	     (car (modspec-info.value x))
	     (car (modspec-info.info x))) 
	    (sdoc-package.info package)))
    
  ;; add the rest later..
    
    package))
  

(defun convert-csf-list (the-list)
  (loop for x in the-list
	collecting (typecase x
		     (csf-method (convert-obj x (make-sdoc-method)))
		     (csf-class (convert-obj x (make-sdoc-class)))
		     (csf-typespec (convert-obj x (make-sdoc-typespec)))
		     (csf-variable (convert-obj x (make-sdoc-variable)))
		     (csf-enum (convert-obj x (make-sdoc-enum)))
		     (csf-directive (convert-obj x (make-sdoc-directive)))
		     (t (error "No conversion in class for ~a" (its-name x))))
	))  


(defun find-category (name entry) 
  (let ((mod (tab-entry-module entry)))
    (loop for x in (sdoc-module.content mod)
	  do (when (typep x 'sdoc-category)
	       (when (equal (sdoc-category.name x) name)
		 (return-from find-category x))))
    nil))



(defun figure-out-modules (toplevel-mod)
  "my guess is that it tries to figure out modules and returns
a list of SDOC modules."
  
  (let ((*print-readably* nil))
    
    (format t "Got ~a~%" toplevel-mod)
    (let ((modlist (modspec-toplevel.modules toplevel-mod))
	  (reslist nil))

      (setq reslist (mapcar #'(lambda (x)
				(etypecase x
				  (modspec-module (convert-obj x (make-sdoc-module)))
				  (modspec-package (convert-obj x (make-sdoc-package)))))
			    modlist))
    
      (let ((just-modules nil))
	(dolist (x modlist)
	  (when (typep x 'modspec-module)
	    (push x just-modules)))
      
      
	;;      (loop for x in modlist 
	;;	  collecting (convert-obj x (make-sdoc-module))))
      
	(when-verbose
	    (warn "collecting tab-entries from: ~a" reslist))


	(setf *module-table*
	      (loop for x in just-modules
		    for y in reslist
		    collecting (make-tab-entry :scope (modspec-module.scope x) :module y)))
      
	;; I really hate loop..
	(loop for x in *module-table*
	      for y in just-modules
	      do (let ((cats (modspec-module.categories y)))
		   (when cats
		     (setf (tab-entry-categories x) 
			   (loop for z in cats 
				 collecting (make-tab-catentry 
					     :scope (modspec-category.scopes z) 
					     :category (find-category (modspec-category.name z) x))))
		     ))
	      ))

      ;;    (loop for x in *module-table*
      ;;	do (format t "Categories for ~a is ~a ~%" x (tab-entry-categories x)))
    
      reslist)))


;;; borrowed from Wheeler Ruml
(defun seq-starts-as (text prefix-str)
  "returns non-nil iff TEXT starts as STR"
  (let ((diff-pos (mismatch prefix-str text)))
    (or (null diff-pos)
        (>= diff-pos (length prefix-str)))))

(defun match-to-scope? (scope name file dir isclass)
  (let ((files nil)
	(prefixes nil)
	(classes nil)
	(dirs nil))

    ;;    (format t "Trying to match ~a to e.g ~a ~%" scope name)
    
    (when scope
      (when isclass
	(setf classes (modspec-scope.classes scope))
	(when classes
	  (when (find name classes :test #'equal)
	    (return-from match-to-scope?  t))))

      
      (setq files (modspec-scope.files scope))
      (when files
	(when (find file files :test #'equal)
	  (return-from match-to-scope?  t)))
      
      (setq dirs (modspec-scope.directories scope))
      (when dirs
	(when (find dir dirs :test #'seq-starts-as)
	  (return-from match-to-scope? t)))
      
      
      (setf prefixes (modspec-scope.prefixes scope))
      (when prefixes
	(when (find name prefixes :test #'seq-starts-as)
	  (return-from match-to-scope?)))

      ))
  nil
  )

(defun actual-container-matching (table name file dir isclass)
  ;;  (format t " Looking for ~a in ~a~%" name table)
  (let ((scope nil))
    (dolist (x table)
      (if (typep x 'tab-entry)
	  (setf scope (tab-entry-scope x))
	  (setf scope (tab-catentry-scope x)))
      ;;      (format t "Scope is ~a ~%" scope)
      (when (and scope (match-to-scope? (car scope) name file dir isclass))
	;;	(format t "Got hit on scope ~a in ~a and returns ~a~%" scope table x)
	(return-from actual-container-matching x))
      )))

(defmethod-with-warn find-matching-container (obj table))
;;  (format t "find-matching-container not implemented for ~a ~a~%" 
;;	  (its-name obj)
;;	  (its-name table))
;;  nil)

(defmethod find-matching-container ((obj csf-method) table)
  (let ((where (csf-method.where obj)))
    (when where
      (let* ((name (car (csf-method.name obj)))
	     (file (car (csf-location.file (car (csf-where.location (car where))))))
	     (dir (position #\/ file :from-end t)) )

    (when dir
      (setf dir (subseq file 0 dir)))

    (actual-container-matching table name file dir nil)
    ))))


(def-or-method find-matching-container ((obj (or csf-variable
						 csf-typespec
						 csf-class
						 csf-directive
						 csf-package
						 csf-enum))
					table)

  (let ((name (get-object-name obj))
	(loc-obj (car (slot-value obj 'location))))
    
    (when loc-obj
      (let* ((file (car (csf-location.file loc-obj)))
	     (dir (position #\/ file :from-end t)) )

	(when dir
	  (setf dir (subseq file 0 dir)))
	
	(actual-container-matching table name file dir nil)
	))))


(defmethod find-matching-container ((obj csf-comment) table) 
  (declare (ignore table))
  )

(defun add-to-module (mod-entry obj orig-obj)
  "My guess is that the function adds something to a mod-obj"
  (let ((whichmod (etypecase mod-entry
		    (tab-entry
		     (tab-entry-module mod-entry))
		    (sdoc-module
		     mod-entry)))
	(cats (typecase mod-entry
		(tab-entry (tab-entry-categories mod-entry))
		(t nil))))

    (when-verbose
	(format t "We're going to add ~a to a module ~a with original ~a..~%"
		obj mod-entry orig-obj))
    
    ;; Must find right cat if cats is valid

    (when cats
      (let ((rescat (find-matching-container orig-obj cats)))
	(when rescat
	  (setf rescat (tab-catentry-category rescat))
	  (if (sdoc-category.content rescat)
	      (nconc (sdoc-category.content rescat) (list obj))
	      (setf (sdoc-category.content rescat) (list obj)))
	  (return-from add-to-module)
	  )))
    
    (if (sdoc-module.content whichmod)
	(nconc (sdoc-module.content whichmod) (list obj))
	(setf (sdoc-module.content whichmod) (list obj)))
    ))


(defun doc-object-p (x)
  "Checks whether a CSF-Comment has documentation.
Currently this check is whether * is the first char in the text."
  (let ((text (csf-comment.text x)))
    (when (and text (consp text))
      (when (eql (char (car text) 0) #\*)
	(return-from doc-object-p t)))
    nil))

(defun create-hash-for-doc (doc-objects)
  "Creates and returns an hashtable with all doc-objects with the 
doc-objects' location.file as key"
  (let ((hashtable (make-hash-table :test #'equal))
	(tmp-entry nil)
	(cur-loc nil))
    
    (dolist (x doc-objects) 
      (setq cur-loc  (car (csf-comment.location x)))
      (setq tmp-entry (make-tab-docentry :loc cur-loc 
					 :text  (car (csf-comment.text x))))

      ;;      (format t "Made docentry ~a~%" tmp-entry)
      
      (push tmp-entry (gethash (car (csf-location.file cur-loc)) hashtable)))
    
    hashtable))

(defun filter-comments-from-tree (tree-node &optional predicate)
  "Filters away comments from the tree and returns
the comments which satisfies the optional predicate. It is destructive
to any content-list in tree-node."

  (let ((content-list (cond ((or (typep tree-node 'csf-toplevel)
				 (typep tree-node 'csf-package)
				 (typep tree-node 'csf-class))
			     (slot-value tree-node 'content))
			    (t nil))))

    (when content-list
      (let ((real-objs nil)
	    (ret-objs nil))
	(dolist (x content-list)
	  (cond ((typep x 'csf-comment)
		 
		 (when (and predicate (funcall predicate x))
		   (push x ret-objs)))
		
		((or (typep x 'csf-package)
		     (typep x 'csf-class))
		 
		 (push x real-objs)
		 (setf ret-objs (append (filter-comments-from-tree x predicate)
					ret-objs)))
		(t
		 (push x real-objs))))
	(setf (slot-value tree-node 'content) (nreverse real-objs))
	ret-objs))
    ))

      #||
      (dolist (x (csf-toplevel.content csf-toplvl))
	(if (typep x 'csf-comment)
	    (when (doc-object-p x)
	      (push x doc-objects))
	  (push x actual-data)))
      (setq actual-data (nreverse actual-data))
      ||#

  
(defun process-conversion-data (csf-toplvl mod-toplvl)
  "The main function which does a lot..even before breakfast"

  (let* ((sdoc-toplvl (make-sdoc-toplevel))
	 (sdoc-modules nil)
	 (*module-table* nil)
	 (*active-language* (figure-out-language csf-toplvl))
	 (*current-doc-parser* (get-comment-parser *active-language*))
	 (table-of-doc nil)
	 (catch-all-module nil)
	 (actual-data nil))

    ;; fix language 
    
    ;; we get module data right away
    (when mod-toplvl
      (when-verbose
	  (albert-info "convert> We try to figure out sdoc-modules"))
      (setq sdoc-modules (figure-out-modules mod-toplvl)))


    ;; we need a catch-all category.. 
    (when (or (eq *active-language* :c++)
	      (eq *active-language* :c))
      ;;(warn "Adding catch all")
      (setf catch-all-module (create-sdoc-module "Global" "Global namespace")))
      
    (when-verbose
	(albert-info "convert> going to filter out comments and save doc-strings"))

    ;;    (describe csf-toplvl)
    
    ;; filter out comments (and save the doc-strings)

    (let ((doc-objects (filter-comments-from-tree csf-toplvl #'doc-object-p)))
      ;; cleaned now?
      (setq actual-data (csf-toplevel.content csf-toplvl))

      (when-verbose
	  (albert-info "convert> got ~a doc-objs and ~a actual data"
		  (length doc-objects)
		  (length actual-data)))
      
      (setq table-of-doc (create-hash-for-doc doc-objects)))
      

    
    ;; iterate over top objects and find modules.. 
    (dolist (x actual-data)
      
;;      (when-verbose
;;	 (warn "Checking ~s for ~s" x *module-table*))
      
      (let ((mod (find-matching-container x *module-table*))
	    (y nil))

	;; we need to convert our csf object
	(typecase x 
	  (csf-method    (setq y (convert-obj x (make-sdoc-method))))
	  (csf-class     (setq y (convert-obj x (make-sdoc-class))))
	  (csf-typespec  (setq y (convert-obj x (make-sdoc-typespec))))
	  (csf-enum      (setq y (convert-obj x (make-sdoc-enum))))
	  (csf-variable  (setq y (convert-obj x (make-sdoc-variable))))
	  (csf-directive (setq y (convert-obj x (make-sdoc-directive))))
	  (csf-package   (setq y (convert-obj x (make-sdoc-package))))
	  (t (error "Unknown type ~a when processing conversion data" (its-name x)) nil))
	
	(unless y
	  (error "We need a SDOC object..."))

	(cond (mod
	       (add-to-module mod y x))
	      ;; we want all non-classes/packages in c/c++ in the catch-all
	      ((and catch-all-module (not (or (typep x 'csf-class)
					      (typep x 'csf-package))))
	       ;;(warn "Add catch all..")
	       (add-to-module catch-all-module y x))
				  
	      (t
	       (setf (sdoc-toplevel.content sdoc-toplvl) 
		     (append (sdoc-toplevel.content sdoc-toplvl) (list y)))))
	))

    ;; fix the language
    (setf (sdoc-toplevel.language sdoc-toplvl) (csf-toplevel.language csf-toplvl))
    
    ;; add catch-all to module-list
    (when catch-all-module
;;      (describe catch-all-module)
      (setq sdoc-modules (cons catch-all-module sdoc-modules)))

    ;; add modules now..
    (setf (sdoc-toplevel.content sdoc-toplvl) 
	  (append (sdoc-toplevel.content sdoc-toplvl) sdoc-modules))

    
    (when-verbose
	(albert-info "convert> have got ~a ~a, and ~a modules"
		csf-toplvl mod-toplvl
		(length sdoc-modules)))

    ;;    (setq mod-toplvl nil)

    ;; time to add docs.. but first find objects needing docs

    (let ((documentable-objs (flatten (find-all-documentable-objs (sdoc-toplevel.content sdoc-toplvl))))
	  (loc nil))
      (when-verbose
	  (albert-info "convert> have got ~a doc-objs" (length documentable-objs)))

      (let ((actual-doc-objs (flatten (loop for x in documentable-objs
					    collecting (progn
							 (setq loc (sdoc:get-locations x))
							 (if loc x nil))))))
	;; check for any difference
	;; (format t "Difference is: ~a~%" (set-difference documentable-objs actual-doc-objs :test #'eq))
	(setq documentable-objs actual-doc-objs))
      
      ;;      (format t "Have got ~a doc-objs~%" (length documentable-objs))

      
      (let ((locs nil)
	    (the-result nil))
	(dolist (x documentable-objs)
	  (setq locs (sdoc:get-locations x))
	  (dolist (y locs)
	    (setq the-result (gethash (car (sdoc-location.file y)) table-of-doc))
	    (when the-result
	      (check-through-entries x y the-result)))))
      )

    (maphash #'add-documentation-to-object table-of-doc)
    
    
    ;;    (add-doc-to-sdoc-tree doc-objects sdoc-toplvl)
    
    ;; opt later


    sdoc-toplvl))


(defun add-documentation-to-object (some-key the-info)
  "goes through a doc entry and adds documentation to applicable objects"
  (declare (ignore some-key))

  (let ((the-obj nil))
    ;;  (format t "The info is ~a~%" the-info)
    
    (dolist (our-entry the-info)
      (setq the-obj (tab-docentry-closest our-entry))
      (when the-obj
	;; we skip the first star.. 
	(add-documentation the-obj (subseq (tab-docentry-text our-entry) 1))))
    
    ))


(defun check-through-entries (obj obj-loc entry-list)
  "Checks through entries to find fitting documentation for an obj"
  (let ((obj-start-line (parse-integer (car (sdoc-location.startline obj-loc))))
	(obj-end-line (sdoc-location.endline obj-loc))
	(obj-line-to-check nil)
	(entry-start-line nil)
	;;	(the-file (car (sdoc-location.file obj-loc)))
	(the-closest-loc nil))

    ;;    (setq obj-end-line (sdoc-location.endline obj-loc))
    (when obj-end-line
      ;; we're not nil.. so make us a number..
      (setq obj-end-line (parse-integer (car obj-end-line))))

    (if (and (not (eq (type-of obj) 'sdoc-class)) obj-end-line)
	(setq obj-line-to-check obj-end-line)
	(setq obj-line-to-check obj-start-line))
    
    ;;    (format t "Checking out ~a~%" obj)

    (dolist (x entry-list)
      ;;      (format t "Checking vs Entry ~a~%" x)
      ;;      (describe x)
      ;; an entry should only have one location..
      (setq entry-start-line (parse-integer (car (csf-location.startline (tab-docentry-loc x)))))
      (setq the-closest-loc (tab-docentry-closest-loc x))
      
      (if the-closest-loc
	  (when (<= entry-start-line obj-line-to-check)
	    (let ((its-val (parse-integer (car (sdoc-location.startline the-closest-loc)))))
	      (when (<= obj-line-to-check its-val)
		;;		(format t "~a: Updating comment ~a to obj ~a [~a] at line ~a (~a)~%" 
		;;			the-file entry-start-line obj (get-object-name obj) obj-start-line obj-line-to-check)
		(setf (tab-docentry-closest x) obj)
		(setf (tab-docentry-closest-loc x) obj-loc))))
	  ;; the else
	  (when (<= entry-start-line obj-line-to-check)
	    ;;	  (format t "~a: Joining comment ~a with ~a [~a] on line ~a (~a)~%" 
	    ;;		  the-file entry-start-line obj (get-object-name obj) obj-start-line obj-line-to-check)
	    (setf (tab-docentry-closest x) obj)
	    (setf (tab-docentry-closest-loc x) obj-loc)))
      
      )
    
    ))


(defun find-all-documentable-objs (some-list)
  "Goes recursively down the list, collecting all objects which may
have documentation found in the code."
  (loop for x in some-list 
	collecting
	(typecase x
	  (sdoc-module (find-all-documentable-objs (sdoc-module.content x)))
	  (sdoc-class (cons x (find-all-documentable-objs (sdoc-class.content x))))
	  (sdoc-category (cons x (find-all-documentable-objs (sdoc-category.content x))))
	  (sdoc-package (cons x (find-all-documentable-objs (sdoc-package.content x))))
	  (sdoc-typespec x)
	  (sdoc-method x)
	  (sdoc-variable x)
	  (sdoc-enum x)
	  (sdoc-directive x)
	  (t
	   (warn "Something screwed up when finding doc obj for ~a" x)
	   nil))
	))

;;(trace find-all-documentable-objs)

(defun convert-csf-to-sdoc (csf-tree
			    &key modspec-file mod-top
			    out-file verify (prettify t)
			    (merge t))
  "converts a csf-tree to an sdoc-tree"
  
  (let ((sdoc-xmltool (apispec-xml:make-xml-tool (make-sdoc-factory)))
	(loc-mod-top mod-top)
	(retval nil))

    (when (and (not loc-mod-top) modspec-file)
      (setq loc-mod-top (parse-modspec-file modspec-file)))
 
    (when verify
      (dolist (x csf-tree)
	(verify-object x t)))

    (when merge t)

    (when prettify
	
      (dolist (x csf-tree)
	(prettify-tree x nil)))
    
    (when-verbose
	(albert-info "convert> we have ~a csfs and ~a mods" 
		(if csf-tree (length csf-tree) nil)
		(if loc-mod-top (length loc-mod-top) nil)))

    ;; the actual job
    (setq retval (process-conversion-data (car csf-tree)
					  (car loc-mod-top)))
    
    (when (and out-file retval)
    
      (with-open-file (str (merge-pathnames (pathname out-file))
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
	
	(format str "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>~%")
	(format str "<!DOCTYPE sdoc SYSTEM \"sdoc.dtd\">~%")

	(apispec-xml:print-as-xml retval str sdoc-xmltool)))

    (if retval
	(list retval)
	nil)))

(defun convert-csf-file-to-sdoc (csf-file
				 &key modspec-file mod-top
				 out-file verify (prettify t)
				 (merge t))
  
  "converts a CSF-file to SDOC.  Returns the SDOC-tree or NIL"
  
  (let ((csf-tree (parse-csf-file csf-file)))

    (unless csf-tree
      (return-from convert-csf-file-to-sdoc nil))

    (convert-csf-to-sdoc csf-tree
			 :modspec-file modspec-file
			 :mod-top mod-top
			 :out-file out-file
			 :prettify prettify
			 :verify verify
			 :merge merge)))
    


(establish-comment-parser& :lisp #'parse-javadoc-text)
(establish-comment-parser& :java #'parse-javadoc-text)
(establish-comment-parser& :c++ #'parse-javadoc-text)
(establish-comment-parser& :c #'parse-javadoc-text)
;; make it default as well
(setq *current-doc-parser* #'parse-javadoc-text)
