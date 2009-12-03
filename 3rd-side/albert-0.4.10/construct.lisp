;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

#|

DESC: construct.lisp - builds the system
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :cl-user)


(proclaim '(optimize
	    #+cmu (ext:inhibit-warnings 3)
	    #+sbcl (sb-ext:inhibit-warnings 3)
	    ;;	  (speed 3)
	    (speed 1)
	    ;;(compilation-speed 0)
	    (compilation-speed 2)
	    (safety 3)
	    ;;	  (debug 1)
	    (debug 3)
	    ))

;;; ---------------------------------
;;; Please check this list to ensure that wanted features are selected

(progn
  ;; are we actively developing and want development code (unless we use clisp)
  #-clisp
  (pushnew :sds-devel *features*)

  ;; will spres use a rule-based system, no alternative really. 
  (pushnew :rule-based *features*)

  )

;;; -----------------------
;;; helper functions..

(defvar *load-verbosity* nil)
(defvar *compile-verbosity* nil)

#+:cmu
(setf extensions:*gc-verbose* nil)
#+allegro
(setf excl::stream-buffer-size 8192)

;;(setf cl:*print-case* :downcase)

(defun load-albert ()
  "Loads Albert"

  (format t "~2&Loading and compiling all relevant SDS/Albert files (and a few extra..)~%")
  (let (
	#+(or cmu sbcl) (*compile-print* *compile-verbosity*)
	#+(or cmu sbcl) (*compile-verbose* *compile-verbosity*)
	;;#+cmu (*error-output* o-str)
	;;(*load-verbose* nil)
	#+cmu (ext:*gc-verbose* nil))

    #+asdf
    (progn
      (load "albert.asd" :verbose *load-verbosity*)
      (asdf:oos 'asdf:load-op 'albert))
    #-asdf
    (progn
      #+mk-defsystem
      (progn
	(load "albert.system" :verbose *load-verbosity*)
	(mk:load-system :albert :minimal-load t :compile-during-load t))

      #-mk-defsystem
      (progn
	(warn "Don't know how to load albert without mkdefsys or asdf.")
	(return-from load-albert nil)))

    
  (format t "~&Full SDS/Albert system loaded..~%")))


;;; ---------------------------
;;; The actual loading..


(load-albert)


#+cmu
(when ext:*batch-mode*
  (quit))
