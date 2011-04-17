;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: APISPEC-XML -*-

#|

DESC: apispec/xml.lisp - this is the APISPEC-XML module
Copyright (c) 1998-2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :apispec-xml)

;;; Rewrite later to a metaclass
(defclass xml-class ()
  ((parent :accessor parent-of :initform nil :initarg :parent
	   :documentation "This is experimental, DO NOT RELY ON IT."))
  #-clisp
  (:documentation 
   "This is ever so long an abstract class (not enforced),
and is supposed to be subclassed for all objects that want
xml-functionality."))


;; should stand out in code..

(deftype xml-attr-type ()
  '(member =attr-string= =attr-stringlist=))

(deftype xml-subelem-type ()
  '(member =subelement-string= =subelement-stringlist= 
           =subelement-ptr=    =subelement-ptrlist=))

;; The following structs is something one usually will not tamper with.. 

#+(or allegro)
(defstruct (xml-attr-info (:constructor %make-attr-info (name the-type ptr)))
  (name  :type symbol)
  (the-type :type symbol)
  (ptr :type t))

#-(or allegro)
(defstruct (xml-attr-info (:constructor %make-attr-info (name the-type ptr)))
  name 
  the-type
  ptr)

#+(or allegro)
(defstruct (xml-subelement-info (:constructor %make-sub-info (name the-type ptr)))
  (name :type symbol)
  (the-type :type symbol)
  (ptr :type t))

#-(or allegro)
(defstruct (xml-subelement-info (:constructor %make-sub-info (name the-type ptr)))
  name
  the-type
  ptr)


(defclass xml-tool ()
  ((the-stack :accessor xml-tool.stack
	      :initarg :stack
	      :initform nil
	      :documentation "This stack is used by the parsing functions
to keep track of progress in the tree.")
   
   (top-objects :accessor xml-tool.top-objects
		:initarg :top-Obj
		:initform nil
		:documentation "The list of the top-objects found
in a parse.  These objects are usually the root of trees.")
   
   (the-factory :accessor xml-tool.factory
		:initarg :factory
		:documentation "A pointer to an object of type XML-Factory
which has been specialised on with produce-xml-object.")
	       
   (want-content :accessor xml-tool.want-content
		 :initarg :want-content
		 :initform nil
		 ;;		:type boolean
		 :documentation "initially nil and should be marked non-nil
if it expects textual content.")

   (indent-level :accessor xml-tool.indent
		 :initarg :indent
		 :type fixnum
		 :initform 0
		 :documentation "is used by the print-as-xml function")
   
   (who-content :accessor xml-tool.who-content
		:initarg :who
		:initform ""
		:documentation "Can't really remember.. oops! :-)")
   )
  #-clisp
  (:documentation "This class should be subclassed for every
domain that needs xml-support.  This is the key class of the XML
system as it provides the factory for constructing the actual obects
and for providing a place to keep the tree."))


(defclass xml-factory ()
  ((name :accessor factory.name
	 :initarg :name
	 ))
  (:documentation "This class should be subclassed by a
class specific for the domain which will be used to specialise
the object creation function."))  

(defgeneric print-as-xml (xmlobj stream xtool)
  (:documentation "This function can be specialised for individual
objects (xmlobj).  The xtool argument should be an object of class
XML-Tool."))

(defgeneric init-attributes (xmlobj attrs)
  (:documentation "This generic function is called add attributes
in an alist to an already created object.  The default implementation
has a slightly complicated functionality and one should probably
mostly limit onseself to :AFTER methods."))

(defgeneric element-start (xmlobj name attrs xtool)
  (:documentation "This function makes sure that the xmlobj is added
in the right place in it's parent.  It should return the xmlobj.
See initAttributes for advice on how to proceed with implementation."))

(defgeneric element-content (xmlobj content xtool)
  (:documentation "This function deals with textual content being
added to an element.  See init-Attributes for advice on
how to proceed with the implementation."))

(defgeneric element-end (xmlobj name xtool)
  (:documentation "Returns a boolean value indicating whether it
is ending or not.  See basic implementation."))

(defgeneric produce-xml-object (factory classname)
  (:documentation "The default implementation will give an error, and
this function should be implemented for each factory-class you use.
It returns a fresh object of class XML-Class.  The classname argument
is a simple-base-string."))

(defgeneric parse-element-start (xtool name attrs)
  (:documentation "This function which has default implementation
can be specialised on the xml-tool and is also given a name of
the new object and an alist of attrs. It is supposed to initialise
topObjects and the stack of the xml-tool as well.  It is meant
to call element-Start and then push the object returned from
element-Start onto the stack in xtool."))

(defgeneric parse-element-end (xtool name)
  (:documentation "This function should pop off the top
of the stack in xtool of element-End returns non-nil. "))

(defgeneric parse-element-content (xtool content)
  (:documentation "This function should call element-Content if
the wantc-ontent flag in xtool is non-nil."))

