;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: albert -*-

#|

DESC: tools/various.lisp - various high-level testing stuff
Copyright (c) 1998-2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :albert)

#+sds-devel
(defun gen-all-apis ()
  "generates all APIs, relative to root in source tree"
  
  (let ((csf-tree (parse-apispec-file "../../apis/csfapi.xml"))
	(sdoc-tree (parse-apispec-file "../../apis/sdocapi.xml"))
	;;(prefs-tree (parse-apispec-file "../../apis/prefsapi.xml"))
	(mod-tree (parse-apispec-file "../../apis/modapi.xml"))
	(api-tree (parse-apispec-file "../../apis/apiapi.xml"))
;;	(javaml-tree (parse-apispec-file "../../apis/javamlapi.xml"))
	(*verbose-operation* t))

    (generate-api csf-tree    :lang "lisp" :out-file "specs/csf-base.lisp")
    (generate-api sdoc-tree   :lang "lisp" :out-file "specs/sdoc-base.lisp")
    ;;(generate-api prefs-tree  :lang "lisp" :out-file "specs/prefs-base.lisp")
    (generate-api mod-tree    :lang "lisp" :out-file "specs/mod-base.lisp")
    (generate-api api-tree    :lang "lisp" :out-file "apispec/api-base.lisp")
;;    (generate-api javaml-tree :lang "lisp" :out-file "specs/javaml-base.lisp")

    (generate-api csf-tree
		  :lang "java"
		  :out-dir "../java/sds/api/csf")

    ;;(generate-api prefs-tree :lang "python" :out-dir "../python/sds/api/prefs")

    
;;    (generate-api javaml-tree
;;		  :lang "java"
;;		  :out-dir "../java/sds/api/javaml")

    
    (generate-api csf-tree
		  :lang "cpp"
		  :decl-file "../../include/csf.H"
		  :src-file "../cplusplus/csf/code.cpp")
    
    (generate-api sdoc-tree
		  :lang "cpp"
		  :decl-file "../../include/sdoc.H"
		  :src-file "../cplusplus/sdoc/code.cpp")
#||    
    (generate-api prefs-tree
		  :lang "cpp"
		  :decl-file "../../include/prefs.H"
		  :src-file "../cplusplus/prefs/code.cpp")
||#    
    
    (generate-api mod-tree
		  :lang "cpp"
		  :decl-file "../../include/mod.H"
		  :src-file "../cplusplus/modspec/code.cpp")

 
    
    ))


#+bananer-fra-foobar
(defun htm ()
  "quick thing to test html output of apis."
  
  (let ((base-path "/home/stig/Projects/sds/")
	(program "generate_api")
	(args '(("apis/csfapi.xml" "doc/apis/csf.html")
		("apis/sdocapi.xml" "doc/apis/sdoc.html")
		;;("apis/prefsapi.xml" "doc/apis/prefs.html")
		("apis/apiapi.xml" "doc/apis/api.html")
		("apis/modapi.xml" "doc/apis/mod.html"))))

    (dolist (i args)
      (albert:run-known-program base-path
				program
				"-v" "-l" "html" "-r" (car i) "-c" (cadr i)))

    ))
