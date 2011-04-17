;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: packages.lisp - all defpackage declarations
Copyright (c) 1998-2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :cl-user)


(defpackage :apispec-base
#+(or clisp lisp2csf)  
  (:documentation "The global SDS package with material used
in a lot of SDS-tools and packages.")
  (:nicknames :apispec)
  (:use :common-lisp #+allegro :clos)
  (:export #:define-sds-module
	   #:def-sds-class
	   #:def-sds-const
	   #:create-obj-constructors
	   #:add-subelements
	   #:add-subelement
	   #:add-attributes
	   #:add-attr
	   #:get-element-name
	   #:create-constructor
	   #:parent-of

	   #:def-or-method
	   #:defmethod-with-warn

	   #:nonboolsym?
	   #:proper-list?
	   #:concat-pnames
	   #:its-name
	   #:strcat
	   #:eat-file
	   #:wipe-file
	   #:verify-object
	   #:verify-object-actual
	   #:register-object
	   #:register-object-actual

	   #:do-iteration
	   #:make-temporary-filename
	   #:run-external-program
	   #:figure-out-fname
	   #:make-sure-dirs-exist
	   #:ensure-dir-name

	   #:*relative-calling-directory*
	   #:*default-albert-output*
	   #:*verbose-operation*
	   #:*submarine-quiet*
	   #:when-verbose
	   #:unless-quiet
	   #:albert-warn
	   #:albert-info
	   
	   #:*xml2esis-prog*
	   #:*xml2sexp-prog*
	   #:*keyword-package*

	   ;; these are common words
	   #:content
	   #:type
	   #:value
	   #:info
	   #:name
	   #:id
	   #:parent
	   #:doc
	   #:defvalue
	   #:passedby
	   #:special
	   #:location
	   #:where
	   #:what
	   #:mod
	   #:access
	   #:language
	   #:text

	   ))
	   

   
(defpackage :apispec-xml
#+(or clisp lisp2csf)  
  (:documentation "Contains all xml-related material of interest to 
various tools and other packages.")
  (:use :common-lisp :apispec-base)
  (:nicknames :api-xml)
  (:export #:xml-class

	   #:attr-info
	   #:sub-info
	   
	   #:xml-subelement-info
	   #:xml-attr-info
	   #:make-xml-attr-info
	   #:make-xml-subelement-info
	   #:%make-attr-info
	   #:%make-sub-info
	   #:make-xml-tool
	   #:factory.name
	   #:create-attribute
	   #:create-subelement
	   #:produce-xml-object
	   #:print-as-xml
	   #:print-indent
   	   #:get-indent
	   #:parse-xml
	   #:parse-typed-xml-file
	   #:xmlify-string

	   #:=attr-string=
	   #:=attr-stringlist=
	   #:=subelement-string=
	   #:=subelement-stringlist=
	   #:=subelement-ptr=
	   #:=subelement-ptrlist=
	   #:xml-tool
	   #:xml-tool.top-objects
	   #:xml-factory))


(defpackage :xml-names
  #+(or clisp lisp2csf)
  (:documentation "a dummy package to dump all
sorts of symbols made during xml'ing")
  (:use)
  (:size 250)
  (:import-from :cl nil))


(defpackage :sds-api-apispec
  #+(or clisp lisp2csf)
  (:documentation "The api for the api-generator")
  (:nicknames #:apispec-api)
  (:export #:make-api-factory
	   #:parse-apispec-file
	   #:generate-api
	   #:generate-api-from-file)
  (:use :common-lisp :apispec-base :apispec-xml))

 
(defpackage :sds-global
#+(or clisp lisp2csf)  
  (:documentation "The global SDS package with material used
in a lot of SDS-tools and packages.")
  (:use :common-lisp #+allegro :clos :apispec-base)
  (:export #:force-to-list
	   #:vector-map
	   #:its-name
	   
	   #:do-stuff-to-symbols
	   #:mac

	   #:subclasses

	   
	   #:convert-list
	   #:flatten
	   #:when-bind
	   #:unless-bind
	   #:positive-integer?
	   #:non-negative-integer?
	   #:filter
	   #:get-object-name
	   #:get-object-id
	   #:get-object-content
	   #:while
	   #:split-seq-on
	   #:split-seq-using
	   #:require-file
	   #:make-decent-filename
	   #:+id-separator+
	   #:%make-id ;; don't use
	   #:%make-id+ ;; don't use
	   
	   #:curry
	   #:rcurry
	   #:compose
	   
	   #:option
	   #:option-keys
	   #:option-has-argument
	   #:option-values
	   #:add-options
	   #:clear-options
	   #:find-option
	   #:parse-options
	   
	   #:list-to-string
	   #:list-to-sep-string

	   #:arrange-duplicates
	   
	   #:prettify-tree
	   #:merge-trees
	   #:merge-tree-list
	   #:*prettification-language*

	   #:verify-id
	   #:verify-idtext

	   #:make-obj-repository
	   #:put-obj-in-repository
	   #:repository.classes
	   #:find-obj-in-rep
	   #:get-tbl-from-rep

	   #:figure-out-language
	   ;; this is hackish..
	   #:csf-toplevel
	   #:sdoc-toplevel

	   #:htbl-to-list
	   
	   #:equal-to

	   #:collect-garbage

	   #:get-info-except-types
	   #:get-info-of-type
	   #:strip-info-fields
	   #:fill-info-obj

	   #:*saved-options*
	   #:+expat-library+
	   #:+nsgmls-prog+
	   #:+xml-repository+

	   #:get-date-string-for-today
	   
	   #:albert-setting
	   #:def-albert-setting
	   #:setting-or-default
	   ))



(defpackage :sds-api-modspec
  #+(or clisp lisp2csf)
  (:documentation "The module specification api")
  (:nicknames #:modspec)
  (:export #:make-modspec-factory
	   #:parse-modspec-file)
  (:use :common-lisp :apispec-base :apispec-xml))

(defpackage :sds-api-csf
  #+(or clisp lisp2csf)
  (:documentation "The csf api")
  (:nicknames #:csf)
;;  (:import-from :sds-global #:get-object-name)
  (:export #:make-csf-factory
	   #:parse-csf-file

	   #:patch-csf-obj
	   #:find-patchable-obj
	   
	   #:csf-buckets
	   #:csf-buckets.classes
	   #:csf-buckets.methods
	   #:csf-buckets.typespecs
	   #:csf-buckets.enums
	   #:csf-buckets.vars
	   #:csf-buckets.comments
	   #:bucket-sort-csf-objs
	   )
  (:use :common-lisp :apispec-base :apispec-xml :sds-global))

(defpackage :sds-api-sdoc
  #+(or clisp lisp2csf)
  (:documentation "The sdoc api")
  (:nicknames #:sdoc)
  (:export #:make-sdoc-factory
	   #:get-locations
	   #:get-doc-as-pairs
	   #:parse-sdoc-file
	   #:get-visibility
	   #:create-sdoc-module

	   #:is-generic-fun?
	   #:is-method?)

  (:use :common-lisp :apispec-base :apispec-xml :sds-global))

(defpackage :sds-api-javaml
  #+(or clisp lisp2csf)
  (:documentation "The javaml api")
  (:nicknames #:javaml)
  (:export #:make-javaml-factory)
  (:use :common-lisp :apispec-base :apispec-xml))


(defpackage :spres-impl
  #+(or clisp lisp2csf)
  (:documentation "The SPRES/SDOC implementation")
  (:use :common-lisp :apispec-base :sds-global :sds-api-sdoc)
  
  (:export #:present-book

	   #:?format
	   #:?outdir
	   #:?repository
	   #:?prog-lang
	   #:?language
	   #:?class-hierarchy

	   #:?parent
	   #:?obj

	   #:*clhs-root*
	   #:*rule-trace*
	   #:*special-doc-handlers*
	   #:*installed-languages*
	   #:*documentation-kwd*
	   #:*enclosing-package*
	   
	   #:get-language
	   #:get-format-constr
	   #:make-class-hierarchy
	   #:format.name

	   #:spres-format
	   #:spres-document
	   #:docbook-document
	   
	   #:docbook-page-title
	   ))



(defpackage :spres
  #+(or clisp lisp2csf)
  (:documentation "the presentation api for outsiders")
  (:use :common-lisp :apispec-base :sds-global)

  (:export #:present-sdoc-file
	   #:present-sdoc
	   #:reset-presentation-system
	   ))


(defpackage :albert
  #+(or clisp lisp2csf)
  (:documentation "the package albert is the interface")
  (:use :common-lisp
	:apispec-base
	:sds-global
	:sds-api-apispec
	:sds-api-csf
	:sds-api-sdoc
	:sds-api-modspec
	#+cmu :mp
	#+asdf :asdf
	)
  (:export #:exit-sds
	   #:convert-csf-to-sdoc
	   #:convert-csf-file-to-sdoc
	   #:link-csf-files
	   #:merge-csf-info
	   #:patch-csf-obj
	   #:sds-server
	   #:run-known-program
	   #:read-programs-from-file

	   #:document-files
	   #:document-systems

	   #:albert-setting
	   ))


(defpackage :lisp2csf
  #+(or clisp lisp2csf)
  (:documentation "the lisp front-end")
  (:use :common-lisp
	:apispec-base
	:sds-global
	:sds-api-csf)
  (:export #:dump-as-csf
	   #:check-body-expression
	   #:analyse-body-expression
	   #:*ignorable-calls*
	   #:*cur-collected-calls*
	   #:analyse-object
	   #:analyse-file
	   #:analyse-files
	   #:*current-package*))


;; hack
(in-package :sds-global)

(defvar *albert-setting-keys* (make-hash-table :test #'equal))
(defvar *albert-settings* (make-hash-table :test #'equal))

(defun albert-setting (key)
  "Returns value of an albert-setting."
  (unless (known-albert-setting? key)
    (warn ">> Albert: Trying to get an unregistered setting ~s" key))
  (multiple-value-bind (val f-p)
      (gethash key *albert-settings*)
    ;;(unless f-p
    ;;  (warn "Unable to find albert-setting for ~s" key))
    ;;(unless (equal key '("hyperspec" "root"))
    ;;  (warn ">>Returning ~s for ~s" val key))
    (values val f-p)))

(defun known-albert-setting? (key)
  "Checks if named key is legal."
  (multiple-value-bind (val f-p)
      (gethash key *albert-setting-keys*)
    (declare (ignore val))
    f-p))

(defun (setf albert-setting) (value key)
  (unless (known-albert-setting? key)
    (warn ">> Albert: Adding setting-value ~s to unknown key ~s" value key))
  (setf (gethash key *albert-settings*) value))

(defmacro def-albert-setting (key value &optional desc)
  (declare (ignore desc))
  `(progn
    (setf (gethash ,key *albert-setting-keys*) t)
    (setf (albert-setting ,key) ,value)))

(defun setting-or-default (key default)
  (multiple-value-bind (val f-p)
      (albert-setting key)
    (if f-p
	val
	default)))
