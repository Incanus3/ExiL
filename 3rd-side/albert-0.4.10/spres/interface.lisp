;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: spres -*-

#|

DESC: spres/interface.lisp - the spres entry-point
Copyright (c) 1998-2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres)

(defun print-info-spres (stream)
  "Prints info about the presentation to the given stream."
  
  ;;(format stream "SDOC System Info (~a version ~a)~2%" *sds-version-name* *sds-version*)

  (maphash #'(lambda (key val)
	       (format stream "Doc-handler '~a' -> '~a'~%"
		       key val))
	   spres-impl:*special-doc-handlers*)
  
  (maphash #'(lambda (key val)
	       (format stream "Natural language '~a' -> '~a'~%"
		       key val))
	   spres-impl:*installed-languages*)
  
  (maphash #'(lambda (key val)
	       (format stream "Doc keyword names '~a' -> '~a' ~%"
		       key val))
	   spres-impl:*documentation-kwd*)
  
  (values))


(defun present-sdoc-file (sdoc-file &key (format-spec :all) (language-spec :en))
  
  "Presents a named sdoc-file, se PRESENT-SDOC for more docs."
  
  (let ((sdoc-tree (sdoc:parse-sdoc-file sdoc-file)))
    (unless sdoc-tree
      (warn "Unable to parse sdoc file ~a" sdoc-file)
      (return-from present-sdoc-file nil))
    
    (present-sdoc sdoc-tree
		  :format-spec format-spec
		  :language-spec language-spec)))

(defun present-sdoc (sdoc-tree &key (format-spec :all) (language-spec :en))
  
  "The real entry point to the SDOC madness.  Initialises
most appropriate variables and may seem complex.  It is.

  SDOC-TREE     should be of type SDOC-TOPLEVEL

  FORMAT-SPEC   should be :all or a named format
  LANGUAGE-SPEC should be :en for english
  PREFS-FILE    should name a preference file
  PREFS-TOP     if you already have a preference-tree, pass the
                toplevel object here and PREFS-FILE won't be used.
"

  (flet ((have-dir-or-leave (the-dir)
	   (let ((blah (make-sure-dirs-exist the-dir)))
	     (unless blah
	       (warn "Was unable to create output-directory [~s], returning NIL."
		     the-dir)
	       (return-from present-sdoc nil)))))

  ;; add me later..
;;  (when-verbose
;;      (print-info-spres *standard-output*))
  
  ;; let us find the outdir and make an index there
  (let ((spres-impl:?outdir nil))

    ;; make sure the sdoc-tree is not a list and that it is valid
    (when (consp sdoc-tree)
      (setf sdoc-tree (car sdoc-tree)))

    (unless (typep sdoc-tree 'sdoc:sdoc-toplevel)
      (warn "Did not get a valid sdoc-tree [~a], returning NIL." sdoc-tree)
      (return-from present-sdoc nil))
    
    ;; we should make a content repository and get the programming language
    (let ((spres-impl:?repository (sds-global:make-obj-repository))
	  (spres-impl:?prog-lang (sds-global:figure-out-language sdoc-tree))
	  (spres-impl:?language (spres-impl:get-language language-spec))
	  (formats (spres-impl:get-format-constr format-spec))
	  (spres-impl:?class-hierarchy nil))

      (unless formats
	;; hack.. fix later
	(setq formats (spres-impl:get-format-constr :all)))

      ;; assume correct format-constructors, get the actual formats
      (setq formats (mapcar #'funcall formats))
      
      ;; register tree in repository
      (typecase sdoc-tree
	(cons (dolist (i sdoc-tree) (register-object i spres-impl:?repository)))
	(sdoc-toplevel (register-object sdoc-tree spres-impl:?repository)))

      (let ((top (if (consp sdoc-tree)
		     (first sdoc-tree)
		     sdoc-tree)))
	(assert (typep top 'sdoc-toplevel))
	(spres-impl::update-parent-status! top nil))
      
      ;; it's handy to have a class-hierarchy
      (setq spres-impl:?class-hierarchy
	    (spres-impl:make-class-hierarchy (repository.classes
					      spres-impl:?repository)))

      ;; time to fix called-by
      (when (albert-setting '("albert" "presentation" "funcallable" "calledby"))
	(spres-impl::update-calledby-info! sdoc-tree))
      
      ;;(spres-impl::prt-hier spres-impl:?class-hierarchy)
;;      (warn "We got far [~s, ~s, ~s, ~s]"
;;	    formats spres-impl::?class-hierarchy
;;	    sdoc-tree spres-impl::?language)

      ;; time to iterate through formats we have and do presentation
      (dolist (i formats)
	(let ((spres-impl:?format i)
	      (spres-impl:?outdir (spres-impl::tl-find-out-dir i)))

	  (apispec-base:when-verbose
	      (apispec-base:albert-info "spres> Will try to write ~s to ~s" (spres-impl:format.name i) spres-impl:?outdir))
	  ;; leave if the outdir is unavailable
	  (have-dir-or-leave spres-impl:?outdir)

	  ;; we boldly assume we want a big book with reference at this stage
	  (spres-impl:present-book sdoc-tree)
	  
	  ))
      
      ))
    ))
