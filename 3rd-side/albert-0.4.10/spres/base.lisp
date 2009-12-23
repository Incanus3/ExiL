;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/base.lisp - the central part of spres package
Copyright (c) 1998-2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

-------

ADD_DESC: This is the main file of the SPRES package..

|#

(in-package :spres-impl)

;; abstract, add docs
(defclass spres-format ()
  ((name :initarg :name :accessor format.name :initform nil)))

;; add docs
(defclass spres-document () 
  ((directory :initarg :directory
	      :initform nil
	      :accessor document.directory)
   
     (filename :initarg :filename
	       :initform nil
	       :accessor document.filename)
   
     (format :initarg :format
	     :accessor document.format)
   
    
     (lang-words :initarg :lang-words
		 :accessor document.lang-words
		 :initform (make-hash-table :test #'equal));; where we have words to use

     ;; very simple way of doing it
     (title :accessor document.title :initform "")

     (content-stream :accessor document.content :initform (make-string-output-stream))

     (style :accessor the-style
	    :initform :brief)
   
     ;;   (subdocuments :accessor document.subdocuments
     ;;		 :initform nil)
     ) )

;; add docs
(defstruct idx-entry
  name obj parent)

;; add docs
(defstruct file-info
  id
  dir
  fname
  counter)

;; add docs
(defclass tree-node ()
  ((the-class :initform nil :initarg :the-class :accessor tnode.class)
   (parents :initform nil :accessor tnode.parents)
   (kids :initform nil :accessor tnode.kids)
   (scope-parent :initform nil :initarg :scope :accessor tnode.scope)
   ))

(defun make-tree-node (class-obj &key (scope nil))
  "constructor"
  (make-instance 'tree-node :the-class class-obj :scope scope))


(defmethod print-object ((inst tree-node) stream)
  (print-unreadable-object (inst stream :identity t)
    (format stream "~:(~S~) [~S]" (class-name (class-of inst)) 
	    (slot-value inst 'the-class)))
  inst)

(defmethod print-object ((inst spres-format) stream)
  (print-unreadable-object (inst stream :identity t)
    (format stream "~:(~S~) [~S]" (class-name (class-of inst)) 
	    (slot-value inst 'name)))
  inst)


;;; === start generics..

(defgeneric mergable-objs? (doc first-obj second-obj)
  (:documentation "Decides whether two objects may be
merged for presentation."))

(defgeneric is-empty? (obj)
  (:documentation "Checks whether an object is empty"))

(defgeneric taggify (format type str)
  (:documentation "Tagifies/Formats given string according to format and type"))

(defgeneric make-document (dir filename format lang)
  (:documentation "creates a document which is a subclass of spres-document"))

(defgeneric present-document (doc &key content-prefix content-suffix)
  (:documentation "Presents a document"))

;; default method in object.lisp
(defgeneric present-object (obj doc style)
  (:documentation
   "Presents an object in a given document (with format and a language) with given
preferences. A classic multi-method"))

;; default method is in search.lisp
(defgeneric search-for-types-in-sdoc-tree (sdoc-tree type cur-path)
  (:documentation
   "Given an sdoc-tree (anywhere), search for objects which satisfies given
type (should be a symbol). Result is returned as a list of conses:
 [obj . belongs-to] where belongs-to is a list giving a trace back to starting
point (given with cur-path which iniially should be nil)"
   ))

(defgeneric get-simple-link (doc dest desc &key hovertext)
  (:documentation "returns a simple link as text where it makes sense"))

(defgeneric make-obj-link (doc obj parent &key desc &allow-other-keys)
  (:documentation "Returns a link to the given object."))

(defgeneric get-simple-anchor (doc anchor-word)
  (:documentation "returns an anchor where appropriate"))

(defgeneric make-obj-id (doc obj parent &key &allow-other-keys)
  (:documentation "Returns an id for the given object."))

(defgeneric get-newline (doc)
  (:documentation "returns a newline for given doc"))

(defgeneric generate-tag (doc tag end-tag?)
  (:documentation "generates some tag"))

(defgeneric present-table (doc objlist style)
  (:documentation "Makes a table of the objects in list in given style"))

(defgeneric calculate-file-list (object table)
  (:documentation "recurses down and collects file-names"))

(defgeneric print-class-hierarcy (doc hierarchy indent)
  (:documentation "Prints a class hierarchy to the given document."))

(defgeneric get-suggested-file-name (object context)
  (:documentation "Asks an object what it wants to have as its
filename.  The method should return a CONS where the CAR part
is the textual id of the object and the CDR part is the filename
it wants."))

(defgeneric should-have-individual-file-p (object context)
  (:documentation "checks to see if the object should have
an individual file"))

(defgeneric collect-indexable (obj)
  (:documentation "Collects all indexable contents of obj."))

(defgeneric update-parent-status! (obj parent)
  (:documentation "Updates OBJ (with parent PARENT) recursively downwards
with correct PARENT-OF info."))

(defgeneric almost-empty-obj? (obj)
  (:documentation "hackish way to check whether it is worth
wasting time on a full entry"))

(defgeneric print-purpose (obj doc context)
  (:documentation "Prints 'purpose' info for OBJ to DOC in a certain context."))

(defgeneric print-header (obj doc context)
  (:documentation "Prints header for a given OBJ to DOC in a certain context."))

(defgeneric get-file-extension (document)
  (:documentation "Returns a string, e.g '.foo' with extension for DOCUMENT files."))

(defgeneric present-objs-in-package (doc package objs kind way)
  (:documentation "Presents OBJS in PACKAGE to DOC of a certain 
KIND (keyword) in a certain WAY (keyword)."))

(defgeneric look-for-name-in (obj name type)
  (:documentation "Looks for an object with name NAME in OBJ matching a TYPE (keyword).
Returns a cons (obj . where-found)."))

(defgeneric recursively-lookup-name (from name type)
  (:documentation "Recursively looks for NAME from object FROM of type TYPE (keyword), 
returns a cons (obj . where-found)."))

(defgeneric get-enclosing-package (obj)
  (:documentation "Returns the enclosing SDOC-PACKAGE for the given object OBJ, or NIL
if not possible."))

(defgeneric get-linked-word (doc word link-type &key desc)
  (:documentation "Returns a linked word fitting for a document object DOC."))

(defgeneric docbook-page-title (doc object)
  (:documentation "Returns a string to use as page-title for an object in docbook, or NIL if
default should be used."))

(defgeneric get-method-arguments (meth-obj prog-lang)
  (:documentation "Returns a string with arguments organised properly for
a method object and a given programming language."))

(defgeneric print-content-list (obj doc content-list)
  (:documentation "Prints the 'content'-list of OBJ to DOC."))

(defgeneric %check-for-method-dispatch (where obj)
  (:documentation "Checks for anything dispathing on OBJ (typically a class or struct)
in WHERE (and WHERE's content). Recursive.  Returns a list of objects."))

(defgeneric put (destination &rest args)
  (:documentation "Puts the ARGS sequentially to the destination which should
be an output-stream of some kind, or a document."))

(defgeneric put-t (destination type &rest args)
  (:documentation "Puts the ARGS sequentially to the destination inside a tag
of type TYPE.  The destination should be an output-stream of some kind,
or a document."))

(defgeneric present-book-header (doc actual-stream)
  (:documentation "Presents a document-header (book) for a document DOC to
the output-stream ACTUAL-STREAM."))


(defgeneric present-book-footer (doc actual-stream)
  (:documentation "Presents a document-footer (book) for a document DOC to
the output-stream ACTUAL-STREAM."))



;;; === end generics

;;; === start base defmethods

(defmethod is-empty? (obj)
  (warn "No IS-EMPTY? function written for ~a" (its-name obj))
  t)

(defmethod taggify (format type str)
  (declare (ignore str))
  (warn "No support have been added for (taggify ~a ~a ..)" (its-name format) type)
  "")


(defmethod make-document (dir filename format lang)
  (declare (ignore dir filename))
  (warn "No support has been written for (MAKE-DOCUMENT dir filename ~a ~a)" 
	(its-name format) (its-name lang)))

;;(defmethod-with-warn present-document (doc))
(defmethod present-document (doc &key content-prefix content-suffix)
  (warn "No support have been written for (PRESENT-DOCUMENT ~a ..)" (its-name doc))
  nil)


(defmethod get-simple-link (doc dest desc &key hovertext)
  (declare (ignore dest desc hovertext))
  (warn "No support have been written for (GET-SIMPLE-LINK ~a ..)" 
	(its-name doc)))


(defmethod make-obj-link (doc obj parent &key desc)
  (declare (ignore doc obj parent desc))
  (error "simple MAKE-OBJ-LINK not implemented."))


(defmethod make-obj-id (doc obj parent &key)
  (declare (ignore doc obj parent))
  (error "simple MAKE-OBJ-ID not implemented."))



(defmethod get-simple-anchor (doc word)
  (declare (ignore word))
  (warn "No support have been written for (GET-SIMPLE-ANCHOR ~a ..)" 
	(its-name doc)))


(defmethod get-newline (doc)
    (warn "No support have been written for (GET-NEWLINE ~a ..)" 
	(its-name doc)))


(defmethod generate-tag (doc tag end-tag?)
  (declare (ignore tag end-tag?))
  (warn "No support have been written for (GENERATE-TAG ~a ..)" 
	(its-name doc)))

(defmethod calculate-file-list (object table)
  (declare (ignore table))
  (warn "No CALCULATE-FILE-LIST written for ~a [~a]" (its-name object) object))


(defmethod print-class-hierarcy (doc hierarchy indent)
  (declare (ignore doc hierarchy indent))
  (error "PRINT-CLASS-HIERARCHY not implemented"))


(defmethod-with-warn get-suggested-file-name (object context))



(defmethod should-have-individual-file-p (object context)
  (declare (ignore object context))
  nil)


(defmethod collect-indexable (obj)
  (albert-warn "spres> unhandled indexable-obj ~s" obj)
  nil)


(defmethod docbook-page-title (doc object)
  (declare (ignore doc object))
  nil)


(defmethod put ((str cl:stream) &rest args)
  "convenient function when dumping output to a document"

  (dolist (h args)
    (when h
      (etypecase h
	(string (write-string h str))
	(integer (format str "~d" h)))))
  (force-output str))


(defmethod put ((doc spres-document) &rest args)
  "convenient function when dumping output to a document"
  (apply #'put (document.content doc) args))

(defmethod put-t ((str cl:stream) type &rest args)
  "puts tagged info to given document"
   
  (dolist (h args)
    (when (and h (length h))
      (write-string (the simple-base-string (taggify doc type h)) str)))
  ;;(terpri str)
  (force-output str))


(defmethod put-t ((doc spres-document) type &rest args)
  (apply #'put-t (document.content doc) type args))

(defmacro with-some-tag (doc tag &rest body)
  `(progn
     (put ,doc (generate-tag ,doc (string ',tag) nil))
     ,@body
     (put ,doc (generate-tag ,doc (string ',tag) t))
     ))

(defmacro itemized-list (doc &rest content)
  `(with-some-tag ,doc ul ,@content))

(defmacro item-in-list (doc &rest content)
  `(with-some-tag ,doc li ,@content))

(defun make-format (format-type)
  "constructor for formats.. supply a symbol identifying the format-type"
  (make-instance format-type))

;;(defmacro fprint (form str &rest args)
;;  `(setf (document-content ,form) (concatenate 'string (document-content ,form) (format nil ,str ,@args))))

(defmacro get-string (acc obj)
  "handy macro for lists who are just a text-string.. for readability"
  `(car (,acc ,obj)))

;;(defmacro eol () (string #\Newline))
(defun eol () (string #\Newline))


;; if you want extra abilities for any format.. just do the
;; same
(defmacro define-format (name key)
  (let ((constr-name (concat-pnames "make-" (string `,name))))
;;    (warn "Defining format with constructor ~a" constr-name)
    `(progn
       (defclass ,name (spres-format) ())
       (defun ,constr-name () (make-instance ',name :name ,key))
       (setf (gethash ,key *available-formats*) #',constr-name))))
  
(defun get-format-constr (the-format)
  "Tries to find the given format and returns a list of constructors that
satisfy the string naming THE-FORMAT.  If the format was not found,
NIL is returned.  If the format is named \"all\", all formats are
returned."

  (when-verbose
    (albert-info "spres> getting format ~a and hashtable has size ~a" 
	    the-format (hash-table-count *available-formats*)))
  
  (if (string-equal (string the-format) "all")
      (htbl-to-list *available-formats*)
    (multiple-value-bind (val found)
	(gethash (string-downcase (string the-format)) *available-formats*)
      (if found
	  (list val)
	(progn
	  (warn "Format ~a not found" the-format)
	  nil)))))




(define-format format-docbook "docbook")
(defclass docbook-document (spres-document) () )



(def-or-method register-object-actual ((obj (or sdoc-class
						sdoc-package
						sdoc-module))
				       
				       context when)
  
  (put-obj-in-repository obj context :scope ?parent)
  (let ((?parent obj))
    (dolist (x (slot-value obj 'content))
      (register-object-actual x context when))))
						

(def-or-method register-object-actual ((obj (or sdoc-toplevel
						sdoc-category))
				       context when)
  
  (dolist (x (slot-value obj 'content))
    (register-object-actual x context when)))
						


(def-or-method register-object-actual ((obj (or sdoc-method
						sdoc-enum
						sdoc-variable
						sdoc-typespec))
						
				       context when)
  (put-obj-in-repository obj context))  
						
(defmethod register-object-actual ((obj sdoc-directive) context when)
  ;; do nothing
  (declare (ignore context when))
  )

(defun register-created-id (obj id)
  "Registers a created id for an obj."
  (let ((table ?idx-register))
    (setf (gethash obj table) id)))

(defun check-for-id (obj)
  "checks if there is an id for obj."
  (let ((table ?idx-register))
    (gethash obj table)))

;;(trace check-for-id)
;;(trace register-created-id)
;; rule-related definitions

;;(defmacro do-rule (name)
;;  `(,name ?obj ?doc ?context))


(defmacro rule-notify-start (key)
  `(when *rule-trace*
    (albert-info "spres> Rule {~a} executed." ,key)))

(defun add-spres-flag! (flag)
  (pushnew flag *spres-flags*))

(defun has-spres-flag? (flag)
  (find flag *spres-flags*))

(defun remove-spres-flag! (flag)
  (setf *spres-flags* (delete flag *spres-flags*)))

(defgeneric is-exported? (obj name)
  (:documentation "Goes upwards in chain to check if given name is exported from package/class/whatever."))
