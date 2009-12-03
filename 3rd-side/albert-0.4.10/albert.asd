;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#||

albert.asd - system definition of Albert
Copyright (c) 1998-2000, 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

||#
(in-package :cl-user)

;; hack
(defun albert/get-late-bind-function (package name)
  "Tries to find a function that may not exist at read, compile
or load time, ie totally dynamic."

  (let* ((pack (find-package package))
         (sym (find-symbol (symbol-name name) pack)))
    (when (fboundp sym)
      (fdefinition sym))))


(defpackage :albert-system 
  (:use :cl :asdf))

(in-package :albert-system)

;; here we decide whether to use optimised printing code
#+(or allegro clisp cmu lispworks sbcl)
(pushnew :optim-print *features*)

;; here we decide whether to use the sexp reader for xml
;; giant boon in clisp, ok in cmucl, slower in acl but saves mem
#+(or allegro clisp cmu lispworks sbcl)
(pushnew :xml-sexp-reader *features*)


(defclass rule-file (cl-source-file) ())

(defmethod perform ((op load-op) (component rule-file))
  ;;(format t "~&compile ~s ~s~%" component *default-pathname-defaults*)
  (let ((*package* (find-package :spres-impl)))
    (funcall (cl-user::albert/get-late-bind-function 'spres-impl 'generate-rule-code)
	     "spres/generated.lisp")
    (compile-file "spres/generated.lisp")
    (load "spres/generated")))

(asdf:defsystem :albert
    :version "0.4.9"
    :author "Stig E Sandø"
    :licence "GPL"
    :properties ((#:author-email . "stig@users.sourceforge.net")
		 ((#:albert #:output-dir) . "Docs-Albert/")
		 ((#:albert #:presentation #:funcallable #:calledby) nil)
		 ((#:albert #:docbook #:cvs-viewurl) . "http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/albert/albert/")
		 ((#:albert #:docbook #:cvs-tag) . "HEAD")
		 )
    :components ((:module package-info
			  :pathname ""
			  :components ((:file "packages")
				       (:file "settings" :depends-on ("packages"))))
		 
		 (:module api-basic
			  :pathname "apispec/"
			  :components ((:file "base")
				       (:file "helpers" :depends-on ("base"))
				       (:file "xml" :depends-on ("helpers"))
				       (:file "xml-base" :depends-on ("xml"))
				       ;; we load both because we want both in library
				       (:file "xml-esis" :depends-on ("xml-base"))
				       (:file "xml-sexp" :depends-on ("xml-base")))
			  :depends-on (package-info))
     
		 ;; this part is only really needed when one wants to
		 ;; generate APIs, not for actual use. 
		 (:module api-generate
			  :pathname "apispec/"
			  :components ((:file "api-base")
				       (:file "api-extra" :depends-on ("api-base"))
				       (:file "api-generate" :depends-on ("api-extra"))
				       (:file "api-lisp" :depends-on ("api-generate"))
				       (:file "api-java" :depends-on ("api-generate"))
				       (:file "api-cpp" :depends-on ("api-generate"))
				       (:file "api-python" :depends-on ("api-generate"))
				       (:file "api-html" :depends-on ("api-generate")))
			  :depends-on (api-basic)
			  )

		 
		 (:module basic
			  :pathname "base/"
			  :components ((:file "tools")
				       (:file "global" :depends-on ("tools")))
			  :depends-on (api-basic))

		   (:module csf
			    :pathname "specs/"
			    :components ((:file "csf-base")
					 (:file "csf-extra"
						:depends-on ("csf-base"))
					 (:file "csf-verify"
						:depends-on ("csf-base"))
					 (:file "csf-prettify"
						:depends-on ("csf-base")))
			    :depends-on (basic))
		   
		    (:module sdoc
			    :pathname "specs/"
			    :components ((:file "sdoc-base")
					 (:file "sdoc-extra"
						:depends-on ("sdoc-base"))
					 (:file "sdoc-prettify"
						:depends-on ("sdoc-base")))
			    :depends-on (basic))

		    (:module modspec
			    :pathname "specs/"
			    :components ((:file "mod-base")
					 (:file "mod-extra"
						:depends-on ("mod-base")))
			    :depends-on (basic))


		   (:module lisp2csf
			    :pathname "lisp2csf/"
			    :components ((:file "cleaning")
					 (:file "borrowed")
					 (:file "lisp2csf" :depends-on ("cleaning" "borrowed")))
			    :depends-on (basic csf))
					 
	 
		 (:module spres-base
			  :pathname "spres/"
			  :components ((:file "vars")
				       (:file "base" :depends-on ("vars"))
				       (:file "tools" :depends-on ("vars" "base"))
				       (:file "files" :depends-on ("vars" "base"))
				       (:file "hyperspec" :depends-on ("vars"))
				       (:file "hier" :depends-on ("base"))
				       (:file "lang" :depends-on ("hier"))
				       (:file "object" :depends-on ("lang" "files" "tools" "hyperspec")))
			  :depends-on (sdoc))
		 
		 ;; this should not be included in production
		 #+sds-devel
		 (:module spres-rule-engine
			  :pathname "spres/rules/"
			  :components ((:file "r-lang")
				       (:file "basic" :depends-on ("r-lang"))
				       (:file "package" :depends-on ("basic"))
				       (:file "class" :depends-on ("basic"))
				       (:file "method" :depends-on ("basic"))
				       (:file "variable" :depends-on ("basic"))
				       (:file "category" :depends-on ("basic"))
				       (:file "sort" :depends-on ("basic"))
				       (:file "index" :depends-on ("basic"))
				       )
			  :depends-on (spres-base))
		 
		 (:module spres-generated
			  :pathname "spres/"
			  :components ((:file "r-db")
				       #+sds-devel
				       (:rule-file "generated" :depends-on ("r-db"))
				       #-sds-devel
				       (:file "generated" :depends-on ("r-db")))
			  :depends-on (spres-base #+sds-devel spres-rule-engine))
		 
		 (:module spres-interface
			  :pathname "spres/"
			  :components ((:file "configure")
				       (:file "interface"))
			  :depends-on (spres-generated))

		 (:module toolbox
			  :pathname "tools/"
			  :components ((:file "base")
				       (:file "linker")
				       (:file "convert" :depends-on ("base"))
				       ;;(:file "programs")
				       ;;#+sds-devel
				       ;;(:file "various")
				       (:file "sds-asdf" :depends-on ("convert"))
				       )

			  :depends-on (basic csf sdoc
					     modspec spres-interface
					     lisp2csf))

		   ))


