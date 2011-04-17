;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: ApiSpec-Base -*-


#|

DESC: apispec/base.lisp - takes care of basic functionality for the whole system
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :apispec-base)

(defparameter *verbose-operation* t 
  "boolean value to specify if we're verbose or not")

(defparameter *submarine-quiet* nil
  "Will we try to suppress any output, even warnings?")

(defvar *default-albert-output* *standard-output* "Where will info and various albert output go.")

(defvar *relative-calling-directory* "./")

(defmacro when-verbose (&body the-body)
  "DOES NOT RETURN A PROPER VALUE.  AVOID FOR MOST STUFF (partially fixed)"
  `(when *verbose-operation*
     (prog1
	 (progn ,@the-body)
       (force-output))))

(defmacro unless-quiet (&body the-body)
  "Unless *submarine-quiet* is true, it executes THE-BODY."
  `(unless *submarine-quiet*
    ,@the-body))

;; turn into a deftype later
(defun nonboolsym? (sym)
  "returns T if SYM is a symbol but not T or NIL."
  (and sym (not (eq sym t)) (symbolp sym)))

(defun proper-list? (obj)
  "Returns T if OBJ is a proper list or NIL."
  (and (listp obj) (null (last obj 0))))

(defmacro albert-warn (str &rest args)
  "Works as WARN if *submarine-quiet* is NIL."
  `(unless-quiet
    (warn ,str ,@args)))

(defun albert-info (str &rest args)
  "Prints a format-string STR with args ARGS to *default-albert-output* with albert-prefix."
  (prog2
      (format *default-albert-output* "~&Albert: ")
      (apply #'format (cons *default-albert-output*
			    (cons str args)))
    (format *default-albert-output* "~%")))

#||
(eval-when (:compile-toplevel :compile-toplevel :load-toplevel)
  (defmacro strcat (&rest args)
    "Macro for concatenating SIMPLE-BASE-STRINGs"
    #+allegro
    `(concatenate 'string ,@args)
    #-allegro
    `(the simple-base-string (concatenate 'simple-base-string ,@args))
    ))
  
(declaim (inline mystrcat))
(defun mystrcat (x y)
  "Basically catenates strings and tries to stringify arguments to be sure"
  (concatenate 'simple-base-string (the simple-base-string (string x)) 
	       (the simple-base-string (string y))))

#+cmu
(defmacro its-name (obj)
  "convenient way to print out info when developing"  
  `(class-name (class-of ,obj)))

#-cmu
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun its-name (obj)

||#

(defun strcat (&rest args)
  (apply #'concatenate 'string args))

(defun its-name (obj)
  "convenient way to print out info when developing"  
  (class-name (class-of obj)))  

(defun mystrcat (x y)
  "Basically catenates strings and tries to stringify arguments to be sure"
  (concatenate 'string (string x) (string y)))

(defvar *keyword-package* (find-package :keyword))

(defmacro concat-pnames (&rest args) 
  "concatenates strings or symbols and returns an interned
symbol which can be passed to e.g defun (as name of function)."

  (let ((str (gensym))
	(case-fun
	 #+allegro
	  (ecase excl:*current-case-mode*
	    (:case-sensitive-lower   'nstring-downcase)
	    (:case-insensitive-upper 'nstring-upcase))
	  #-allegro
	  (ecase (cl:readtable-case cl:*readtable*)
	    (:downcase 'nstring-downcase)
	    (:upcase 'nstring-upcase))
	  ))

    `(let ((,str (,case-fun (reduce #'mystrcat (list ,@args)))))
       (if (and (plusp (length ,str)) (eql (char ,str 0) #\:))
	   (intern (subseq ,str 1) *keyword-package*)
	   (intern ,str)
	   ))
    ))

#-clisp
(defun eat-file (filename)
  "eats a whole file and returns it as a string.
If the filename argument is a string, it is made into a pathname
and merged with the current path, if it is a pathname it is
used directly."
  
  (declare (optimize (safety 0) (speed 3) (debug 0)))

  (let ((the-file-path (etypecase filename
			 (string (merge-pathnames (pathname filename)))
			 (pathname filename))))
  
    (with-open-file (str the-file-path :direction :input)
      (let ((buffer (make-array (file-length str)
				:element-type (stream-element-type str)
				;; :allocation :old
				)))
	#+clisp
	(system::read-char-sequence buffer str)
      
	;; NOTE poplog has no read-sequence
	#-clisp
	(read-sequence buffer str)

	;; <kmr>   for parsing the results of eat-file, you call octets-to-
	;;  string for (and allegro ics), but this needs to be called for any allegro. also,
	;;  you might want to put that post-processing in eat-file itself

	#+allegro (excl:octets-to-string buffer)
	#-allegro buffer
	))))

;; submitted by s madhu
#+clisp
(defun eat-file (filename)
  "eats a whole file and returns it as a string.
If the filename argument is a string, it is made into a pathname
and merged with the current path, if it is a pathname it is
used directly."

  (declare (optimize (safety 0) (speed 3) (debug 0)))

  (with-open-file (s filename :direction :input :element-type 
		     '(unsigned-byte 8))
    (let* ((len (file-length s))
	   (buf (make-array len :element-type '(unsigned-byte 8)))
	   (pos (read-sequence buf s)))
      (assert (= pos len))
      (map 'string #'code-char buf))))




(defun wipe-file (filename)
  "deletes the file named by filename"
  (let ((fptr (probe-file filename)))
    (when fptr
      (when-verbose
	  (format t "Wiping file ~a~%" filename))
      (delete-file fptr))
    ))



(defgeneric get-element-name (xmlobj)
  (:documentation "Returns a string with the element name of the xml-object"))


(defun figure-out-fname (fname)
  "should be made into one line later.."
  (let* ((fname (pathname fname))
	 (result (merge-pathnames fname)))
;;    (warn "FNAME is ~a vs ~s" fname (namestring result))
    (namestring result)))



;;; this is mean and ugly.. find a better way later
#-(or allegro cmu)
(defvar *tempfile-cnt* 19)
(defun make-temporary-filename ()
  "Returns the name of a temporary filename"
  #+allegro (system:make-temp-file-name)
  #+cmu
  (system::pick-temporary-file-name)
  #-(or allegro cmu) 
  (let* ((num (incf *tempfile-cnt*))
	 (filename (format nil "/tmp/sds-tempfile-~a" num))
	 (retval (pathname filename)))
    filename)
  )

(defun run-external-program (program args &key outfile )
  " Should return the int that the external program returns.. "

;;  (warn "outfile is ~s" outfile)
  
  (let ((the-cmd ""))
    #+(or allegro clisp lispworks)
    (setq the-cmd (concatenate 'string program " " args (if outfile 
							    (concatenate 'string " > " outfile)
							    "")))
    
    #+allegro (excl:run-shell-command the-cmd :wait t)
    #+clisp (ext:shell the-cmd)
    #+lispworks (system::call-system the-cmd)
    #+:cmu (let ((res (extensions:run-program program (list args) 
					      :output outfile :wait t 
					      :if-output-exists :supersede)))
;;	   (warn "Exec of {~a} Res was ~a ~a"
;;		 (list program args outfile)
;;		 res (extensions:process-exit-code res))
	   (extensions:process-exit-code res))

    #+:sbcl (let ((res (sb-ext:run-program program (list args) 
					   :output outfile :wait t 
					   :if-output-exists :supersede)))
	      (sb-ext:process-exit-code res))

    ;; not sure of this one though, fix outfile
    #+poplog (progn
	       (require :run-unix-program)
	       (poplog::run-unix-program program :args args))
  ))


(defgeneric verify-object (obj context)
  (:documentation "Verifies an object given a context to verify under."))

(defgeneric verify-object-actual (obj context when)
  (:documentation "Implementation of VERIFY-OBJECT"))

;;(defmethod-with-warn verify-object (obj context))
(defmethod verify-object (obj context)
  (declare (ignore context))
  (warn "Trying to verify ~a, but can't" (its-name obj)))

(defgeneric register-object (obj context)
  (:documentation "registers the object with the given context"))

(defmethod register-object (obj context)
  (declare (ignore context))
  (warn "No REGISTER-OBJECT written for ~a [~a]" (its-name obj) obj))

(defgeneric register-object-actual (obj context when)
  (:documentation "Implementation of REGISTER-OBJECT"))


(defgeneric do-iteration (node function context &key recursive &allow-other-keys)
  (:documentation "Iteration function which all other quasi-iterative
functions should use. The function argument should take three arguments,
the node, the context and an argument specifying when it was called 
(:before or :after kids are processed for parents or :in for leaves). 
The return value \\underline{can} be a keyword which can have special meaning. 
One such keyword is :cancel which will stop the iteration."))

#||
(defmethod do-iteration ((node cons) function context 
			 &key (recursive t) &allow-other-keys)
  (if recursive
      (dolist (x node)
	(funcall function x context :in))
      (funcall function (car node) context :in))
  
  t)
||#

;;(defmethod-with-warn do-iteration (node function context 
;;					&key (recursive t) &allow-other-keys))

(defmethod do-iteration (node function context 
			      &key (recursive t) &allow-other-keys)
  (declare (ignore recursive))
  (warn "No handler for (DO-ITERATION ~a ~a ~a)" 
	(its-name node) (its-name function) (its-name context)))
  

(defun make-sure-dirs-exist (dir)
  "mostly a call to ENSURE-DIRECTORIES-EXIST,
but stops errors from floating out.. returns NIL instead."
  (let ((the-path (merge-pathnames (etypecase dir
				     (pathname dir)
				     (string (pathname dir))))))

    (handler-case
	(ensure-directories-exist the-path
				  :verbose *verbose-operation*)
      (file-error (co)
	(warn "Something went wrong [~a] during directory creation [~s], returning NIL."
	      co the-path)
	nil))))

;;(trace make-sure-dirs-exist)


(defun ensure-dir-name (str)
  "Makes sure the str has a / suffix"
  (etypecase str
    (string
     (if (and str (not (eq (char str (1- (length str))) #\/)))
	 (concatenate 'string str "/")
	 str))
    ;; bad
    (pathname str)
    ))
