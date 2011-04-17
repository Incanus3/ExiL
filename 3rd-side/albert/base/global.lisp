;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SDS-GLOBAL -*-
#|

DESC: base/global.lisp - main utilities for SDS
Copyright (c) 1998,1999 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

-------

This area is for any global declarations which may show up during
development (not very likely to be many). Functions which are useful 
several places are also put here. Some generic functions might show up 
too (for all I know)

And not least.. this is the area for helpful macros..

The macros are created as we go along and see patterns which can be 
simplified.

Oh and yes.. the (eval-when)'s are paranoid... fix later :-)
|#

(in-package :sds-global)


(defgeneric get-object-name (obj)
  (:documentation "Returns a string with the name of the object"))

(defmethod get-object-name (obj)
  (error "No get-object-name written for ~a" obj))


(defgeneric get-object-id (obj)
  (:documentation "Returns a string with the id of the object"))

(defmethod get-object-id (obj)
  (warn "No get-object-id written for ~a" obj))

(defgeneric get-object-content (obj)
  (:documentation "Returns a list of content when appropriate or NIL."))

(defmethod get-object-content (obj)
  (declare (ignore obj))
  nil)

(defmacro convert-list (src dest constructor)
  "converts all objects in SRC into DEST and uses CONSTRUCTOR 
to create clean objects"
  (let ((conv-name (concat-pnames "" "convert-obj"))
	(x (gensym)))
    `(setf ,dest 
      (loop for ,x in ,src 
       collecting (,conv-name ,x (,constructor))))
    ))




(defvar *prettification-language* nil)

(defgeneric prettify-tree (object parent-list)
  (:documentation "Prettifies an object/tree.  Is allowed to change 
itself, children and parents.  Do not invoke it unless you know 
what you're doing."))


(defmethod prettify-tree (object parent-list)
  (declare (ignore parent-list))
  object)


(defgeneric merge-trees (first second)
  (:documentation "merges two trees and returns one tree"))

(defmethod merge-trees (first second)
  (warn "No merge written for ~a ~a, returning first" 
	(its-name first)
	(its-name second))
  first)

(defun merge-tree-list (trees)
  "takes a list as input and reduces the list to a simple
object through calls to MERGE-TREES."
  (reduce #'merge-trees trees))


(defstruct id-obj
  type
  name
  fullname
  params
  locs
  scope)

(defconstant +id-separator+ #\£)

(defun parse-id (id-text)
  "could do error-checking.. later.." 
  (let ((new-id (make-id-obj))
	(id-list (cdr (split-seq-on id-text +id-separator+))))
    
    (setf (id-obj-type     new-id) (first  id-list))
    (setf (id-obj-name     new-id) (second id-list))
    (setf (id-obj-fullname new-id) (third  id-list))
    (setf (id-obj-params   new-id) (fourth id-list))
    (setf (id-obj-locs     new-id) (cdddr id-list))
    
    new-id))

(defun %make-id (type name fullname params locs scope)
  "HACK!"
  (let ((sep sds-global:+id-separator+)
	(scope (if (and scope (plusp (length scope))) (format nil "[~a]" scope) ""))
	(use-loc nil)) ;; hack
    (if use-loc
	(format nil "~A~a~A~a~A~a~A~a~A~a~A~a~A"
		sep type sep name sep fullname sep params sep locs sep scope sep)
	(format nil "~A~a~A~a~A~a~A~a~A~a~A"
		sep type sep name sep fullname sep params sep scope sep))))
	

(defun %make-id+ (type &key (name "") (fullname "") (params "") (locs "") (scope ""))
  "HACK!"
  (%make-id type name fullname params locs scope))


(defun verify-idtext (id-text)
  "imperative function which painfully goes through the id text. Returns
NIL if the id wasn't any good."
  
  (flet ((warn-leave (msg msg2)
	   (warn "ID [~a] ~a ~a" id-text msg msg2)
	   (return-from verify-idtext nil)))
    
    (let ((len (length id-text))
	  (sep +id-separator+)
	  (text "")
	  (buf "")
	  (pos 0))
      
      (unless (eq (schar id-text 0) sep)
	(warn-leave "starts out wrong... should be" (string sep)))
      
      (unless (eq (schar id-text (- len 1)) sep)
	(warn-leave "ends wrong... should be" (string sep)))
      
      (setq text (subseq id-text 1 (- len 1)))
      
      (setq pos (position sep text))
      (unless pos
	(warn-leave "unable to find second" (string sep)))
      
      (setq buf (subseq text 0 pos))
      
      (unless (member buf '("method" "variable" 
			    "enum" "typespec" "package"
			    "class") :test #'equal)
	(warn-leave "unknown type" buf))
      
      (setq text (subseq text (1+ pos)))
      
      (setq pos (position sep text))
      (unless pos
	(warn-leave "unable to find third" (string sep)))

      (setq buf (subseq text 0 pos))
      
      (unless (length buf)		; we just make sure the name has length
	(warn-leave "the name has no length" ""))
      
      (setq text (subseq text (1+ pos)))
      
      (setq pos (position sep text))
      (unless pos
	(warn-leave "unable to find fourth" (string sep)))

      ;; fullname is optional and lang-specific
      
      (setq text (subseq text (1+ pos)))
      
      (setq pos (position sep text))
      (unless pos
	(warn-leave "unable to find fifth" (string sep)))

      ;; params is also optional.. no point in checking
      
      (setq text (subseq text (1+ pos)))

      ;; text should now just be location(s) which should start with [
      
      ;;(warn "TEXT: ~a" text)
      
      (unless (eq (schar text 0) #\[)
	(warn-leave "location starts out wrong... should be" (string #\[)))
      
      
      ;; do rest of checking later..
      
      t)))


(defun verify-id (obj context)
  "Verifies an object's id-field in a certain context"
  (declare (ignore context))
  (let ((ids (slot-value obj 'id)))
    (dolist (x ids)
      (unless (verify-idtext x)
	(error "Screwed up on id")))))


;; contains hashtables that ties up all objects
(defclass object-repository ()
  ((modules :accessor repository.modules)
   (classes :accessor repository.classes)
   methods
   variables
   typespecs
   packages
   enums))
    
(defun make-obj-repository ()
  "Creates and instantiates an OBJECT-REPOSITORY and returns it"
  
  (let ((rep (make-instance 'object-repository)))
    (dolist (x '(modules
		 classes
		 methods
		 variables
		 typespecs
		 packages
		 enums))
      (setf (slot-value rep x) (make-hash-table :test #'equal)))
    rep))


(defun put-obj-in-repository (obj rep &key (scope nil))
  "Puts given object in repository. Key is the NAME in the id. Value
is a pair where CAR is an ID-OBJ and CDR is the object."
  
  (let* ((id-text (car (slot-value obj 'id)))
	 (id-obj (parse-id id-text))
	 (obj-type (id-obj-type id-obj))
	 (comb-obj (cons id-obj obj))
	 (hashval (id-obj-name id-obj)))

    ;; hack
    (when scope
      (setf (id-obj-scope id-obj) scope))
    
    ;; fix to typecase-later
    (cond
      ((string-equal obj-type "method")
       (push comb-obj (gethash hashval (slot-value rep 'methods))))
      ((string-equal obj-type "variable")
       (push comb-obj (gethash hashval (slot-value rep 'variables))))
      ((string-equal obj-type "class")
       (push comb-obj (gethash hashval (slot-value rep 'classes))))     
      ((string-equal obj-type "enum")
       (push comb-obj (gethash hashval (slot-value rep 'enums))))     
      ((string-equal obj-type "typespec")
       (push comb-obj (gethash hashval (slot-value rep 'typespecs))))     
      ((string-equal obj-type "module")
       (push comb-obj (gethash hashval (slot-value rep 'modules))))
      ((string-equal obj-type "package")
       (push comb-obj (gethash hashval (slot-value rep 'packages))))
      (t
       (warn "No object-rep-placement made for ~a [~a]" obj-type
	     (its-name obj))))))

(defun get-tbl-from-rep (obj-type rep)
  "Gets a table from the repository. The ob-type is a string with the type
of the object. Returns NIL on failure."
  
  (cond
    ((string-equal obj-type "method")   (slot-value rep 'methods))
    ((string-equal obj-type "variable") (slot-value rep 'variables))
    ((string-equal obj-type "class")    (slot-value rep 'classes))     
    ((string-equal obj-type "enum")     (slot-value rep 'enums))     
    ((string-equal obj-type "typespec") (slot-value rep 'typespecs))
    ((string-equal obj-type "module")   (slot-value rep 'modules))
    ((string-equal obj-type "package")   (slot-value rep 'packages))
    (t
     (warn "No get-tbl-from-rep made for ~a" obj-type)
     nil)))

(defun find-obj-in-rep (id table)
  "Tries to find correct object with given id in table.
Returns NIL on failure."
  
  (let ((id-obj (parse-id id)))
    (declare (dynamic-extent id-obj))
    (multiple-value-bind (res f)
	(gethash (id-obj-name id-obj) table)
      (when (and f res)
	(dolist (x res)
	  ;;	  (warn "F ~a" x)
	  (when (string-equal id (car (slot-value (cdr x) 'id)))
	    (return-from find-obj-in-rep (cdr x)))))))
  nil)


(defstruct option
  keys
  has-argument
  (values nil))

(defvar *saved-options* nil)


(defun add-options (keys)
  "specify keys as '((\"-k\" \"--kill\") ARG)"
  (dolist (i keys)
    (let ((opt (make-option)))
      (if (consp (car i))
	  (setf (option-keys opt) (car i))
	(setf (option-keys opt) (list (car i))))
      (if (and (cdr i) (eq (cadr i) :arg))
	  (setf (option-has-argument opt) t)
	(setf (option-has-argument opt) nil))
      (push opt *saved-options*)))
  (values))

(defun clear-options ()
  "clears any set options"
  (setq *saved-options* nil)
  (values))

(defun find-option (key &optional (opt-list *saved-options*))
  "tries to find an option with given key. returns option struct"
  (dolist (i opt-list)
    (let ((keys (option-keys i)))
      (assert (typep keys 'list))
      (let ((found (find key keys :test #'equal)))
	(when found
	  (return-from find-option i)))))
  nil)

(defun parse-options (optlist)
  "parses a list of arguments. 
Returns encountered options and 'rest' arguments."
  
;;  (format t "Got opts: ~a~%" optlist)
  (let ((last-opt nil)
	(rest-args nil))
    
    (dolist (i optlist)
;;      (format t "Opt: ~a~%" i)
      (if last-opt
	  (progn
	    (setf (option-values last-opt) i)
	    (setq last-opt nil))
	(let ((some-opt (find-option i)))
	  (if some-opt
	      (progn
		(if (option-has-argument some-opt)
		    (setq last-opt some-opt)
		  (setf (option-values some-opt) t)))
	    (push i rest-args)))))
    
    (let ((opts (filter #'(lambda (x) (if (option-values x) x nil)) *saved-options*)))
      (values opts (reverse rest-args)))))

(defun %get-day-name (num)
  (nth num '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))

(defun %get-mon-name (num)
  (nth num '("Error"
	     "Jan" "Feb" "Mar" "Apr" "May" "Jun"
	     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

(defun get-date-string-for-today ()
  "Returns a date-string for today."
  (multiple-value-bind (sec min hour date month year day day-p zone)
      (get-decoded-time)
    (declare (ignorable sec min hour date month year day day-p zone))
    (format nil "~a ~a ~a" date (%get-mon-name month) year)))
