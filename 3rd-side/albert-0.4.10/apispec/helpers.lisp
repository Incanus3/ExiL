;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: apispec-base -*-


#|

DESC: apispec/helpers.lisp - helping macros
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :apispec-base)


(defmacro defmethod-with-warn (name arglist)
  "creates a method which prints out a warning that it's not written"
  (let* ((ignorable-args nil)
         (args (reduce #'(lambda (x y) 
			   (declare (type simple-base-string x y))
			   (strcat x " " y)) 
                       (mapcar #'(lambda (x) 
                                   ;; we should ignore non-specialised arguments
                                   (unless (consp x) 
                                     (push x ignorable-args))
                                   ;; return a string
                                   (format nil "~a" (its-name x))) 
                               arglist)))
         (ignore-code (if ignorable-args
                          `(declare (ignore ,@ignorable-args))
                        nil)))
    `(progn
       (defmethod ,name ,arglist
                  ,ignore-code
                  (warn "No handler written for (~a ~a)" ',name ,args)))))

(defvar *fun-counter* 100)

(declaim (type fixnum *fun-counter*))

(unless (boundp '*fun-counter*)
  (setq *fun-counter* 100))

(defmacro def-or-method (name params &body itsbody)
  "defines methods based on OR'ed classes. 
does not handle lambda-lists properly"
  
  (let ((methlist nil)) 
    (labels ((get-signatures (list acc)	; adds to methlist
	       (if (null list)
		 (push acc methlist)
		 (let ((arg (car list)))
		   (if (symbolp arg)
		       (get-signatures (cdr list) (append acc (list arg)))
		     ;; specialised parameter
		     ;; the car should be a regular symbol and the cdr should be a consp
		     (let ((cl-constr (cadr arg)))
		       (dolist (i (cdr cl-constr))
			 (get-signatures (cdr list) 
					 (append acc (list (list (car arg) i)))))))))))
    
      (get-signatures params nil))
    
    (when (> (length methlist) 0)
      ;; we want names of args. we just need one of the methlist
      (let ((arglist (mapcar #'(lambda (a) 
				 (if (symbolp a)
				     a
				   (car a)))
			     (car methlist))))

	;; name of helper
	;; GENSYM is broken in cmucl at least.. probably all of CL
	
	(let ((fun-name (concat-pnames "helperfun-" name "-"
				       (format nil "~a" (incf *fun-counter*)))))
;;	  (warn "Making ~a for ~a" fun-name methlist)
	  `(eval-when (:compile-toplevel 
		       :load-toplevel)
	     (defun ,fun-name ,arglist (block ,name ,@itsbody))
	     ,@(mapcar #'(lambda (x) `(defmethod ,name ,x (,fun-name ,@arglist)))
		       methlist)))))))


;; must be made into a singleton later
(defmacro define-sds-module (mod-name)
  "
Should be the first expr after the defpackage and in-package. It currently
defines three things:
- A constant named +sds-module-name+ with val mod-name
- A hash-table for constructors named *constructors*
- A function get-constructor which takes an argument name and returns
  a constructor from *constructors*
"
  ;;  (warn "defining module ~a" mod-name)
  
  (let ((pack-name (concat-pnames "" "+sds-module-name+"))
	(constr-name (concat-pnames "" "get-constructor"))
	(constr-hash (concat-pnames "" "*constructors*"))
	(name-word (concat-pnames "" "name")))
    
    `(eval-when (:compile-toplevel 
		 :load-toplevel
		 #+clisp :execute)
      (defconstant ,pack-name (string ',mod-name))
      (defvar ,constr-hash (make-hash-table :test #'equal))
      (declaim (inline ,constr-name))
      (defun ,constr-name (,name-word) (gethash ,name-word ,constr-hash)))))


;;(defmacro get-constant-name (name)
;;  (concat-pnames "+" `,*sds-module-name* "-name-" `,name "+"))

;; ugly hack..
(defmacro def-sds-const (name val)
  "Defines a constant (defconstant) named as in (get-const-name) with value val"
  ;;  (warn "defining const ~a" name)
   
  (let* (;;(mod-val (symbol-value (find-symbol (format nil "~a" :+sds-module-name+) *package*)))
	 (sym-name (concat-pnames "+sds-" "module-name+"))
	 (mod-val (symbol-value sym-name))
	 (const-name (concat-pnames "+" mod-val "-name-" `,name "+")))
    `(eval-when (:compile-toplevel 
		 :load-toplevel
		 #+clisp :execute)
      (defvar ,const-name ,val))))

(defmacro get-const-name (name)
  "
 Given NAME it returns the symbol:
   +MOD-name-NAME+
 where MOD is the name of the module
"
  (let* (;;(mod-val (symbol-value (find-symbol (format nil "~a" :+sds-module-name+) *package*)))
	 (sym-name (concat-pnames "+sds-" "module-name+"))
	 (mod-val (symbol-value sym-name))
	 (const-name (concat-pnames "+" mod-val "-name-" `,name "+")))
    `,const-name))

(defmacro add-to-constr-table (const-name fun-name)
  "
adds an entry to the constructor table. First argument should be 
the constant tied to the constructor and the second should be a 
function taking no arguments
"
  (let ((constr-hash (concat-pnames "" "*constructors*")))
    `(setf (gethash (get-const-name ,const-name) ,constr-hash) ,fun-name)))


(defmacro create-basics-for-class (name xml-name)
  "
The name (of the class) forms the basis for all
created functions. The xml-name should be a defconstant which
is a string
"
  (let* (;;(mod-val (symbol-value (find-symbol (format nil "~a" :+sds-module-name+) *package*)))
	 (sym-name (concat-pnames "+sds-" "module-name+"))
	 (mod-val (symbol-value sym-name))
	 (full-class-name (concat-pnames mod-val "-" `,name))
	 ;;(constr-name (concat-pnames "MAKE-" full-class-name))
	 (const-name (concat-pnames "+" mod-val "-name-" `,xml-name "+"))
	 ;;(constr-hash (concat-pnames "" "*constructors*"))
	 )
    `(eval-when (:compile-toplevel 
		 :load-toplevel
		 #+clisp :execute)
		 
      ;;       (format t "creating basics for ~a~%" ',full-class-name)
      ;;       (create-constructor ,full-class-name)
      (create-elemname-fun ,full-class-name ,const-name)
      ;;       (setf (gethash ,const-name ,constr-hash) (function ,constr-name))
      ;;       (format t "hash ~a is now ~a~%" ',constr-hash ,constr-hash)
      )
    ))

(defmacro create-elemname-fun (name retval)
  "
Creates a method get-element-name where name is the class and retval is what is
returned.
"
  (let ((xml-word (concat-pnames "" "xmlobj")))
    `(defmethod get-element-name ((,xml-word ,name)) ,retval)))

(defmacro create-constructor (name)
  "
The macro generally creates a function (constructor) named
MAKE-name (where name is the first argument and name of the class)
and returns an instance of the class.

The constructor is also automagically exported...
"
					;  (format t "Value of name ~a ~a~%" (type-of name) (if (boundp name) (symbol-value name) t))
  (let ((new-name (concat-pnames "MAKE-" name)))
    `(eval-when (:compile-toplevel 
		 :load-toplevel
		 #+clisp :execute)
      (defun ,new-name (&key (parent nil)) (make-instance ',name :parent parent))
      (export ',new-name))
    ))

(defmacro add-subelement (obj type const-name slot)
  "
Is a wrapper for the function create-subelement in APISPEC-XML
but is easier to use. A use may be:
  (add-subelement xm PTRLIST project   toplevel-projects)

where:
  xm is the name of the object var
  PTRLIST is the type (it is transformed into =subelement-ptrlist= for c-s)
  project is the name of the constant with the xml-value
  toplevel-projects is the name of the slot as in CLASSNAME-SLOT
"

  (let* ((fun-name (concat-pnames "create-" "subelement"))
	 (type-name (concat-pnames "=subelement-" `,type "="))
	 ;;	 (mod-val (symbol-value (find-symbol "*SDS-MODULE-NAME*" *package*)))
	 ;;	 (slot-name (concat-pnames mod-val "-" slot))
	 (slot-name (concat-pnames "" slot))
	 )
    ;;    `(,fun-name ,obj (get-const-name ,const-name) ',type-name (,slot-name ,obj))))
    ;;    `(,fun-name ,obj (get-const-name ,const-name) ',type-name (function ,slot-name))))
    `(,fun-name ,obj (get-const-name ,const-name) ',type-name ',slot-name)))


(defmacro add-attr (obj type const-name slot)
  "
Basically works as add-subelement, see it's documentation.
Calls create-attribute instead.

The third argument differs though and is expected to be a simple string
"

  (let* ((fun-name (concat-pnames "create-" "attribute"))
	 ;;	 (mod-val (symbol-value (find-symbol "*SDS-MODULE-NAME*" *package*)))
	 (type-name (concat-pnames "=attr-" `,type "="))
	 ;;	 (cl-name (concat-pnames mod-val "-" `,obj))
	 ;;	 (mod-val (symbol-value (find-symbol "*SDS-MODULE-NAME*" *package*)))
	 ;;	 (slot-name (concat-pnames mod-val "-" slot)))
	 (slot-name (concat-pnames "" slot)))
    ;;    `(,fun-name ,obj ,const-name ',type-name (,slot-name ,obj))))
    `(,fun-name ,obj ,const-name ',type-name ',slot-name)))

(defmacro add-attributes (obj &rest attrs)
  (let ((acc-name (concat-pnames "xml-class." "attr-info-ptr"))
	(fun-name (concat-pnames "make-xml-" "attr-info"))
	#-cmu
	(el-type (concat-pnames "" "xml-attr-info"))
	(howmany (length attrs))
	(insertions nil)
	(cmds nil)
	(type-name nil)
	(tmparr (gensym)))
    
    (dolist (curptr (reverse attrs))
      (if (not (and (consp curptr) (= (length curptr) 3)))
	  (error "Arguments to macro add-attributes should be on the form (type name slot)")
	  (progn
	    (setf type-name (concat-pnames "=attr-" (first curptr) "="))
	    ;;	(format t "Type-name is ~a~%" type-name)
	    (push `(,fun-name ,(second curptr) ',type-name ',(third curptr)) insertions)
	    )))
    
    ;;    (format t "Insertions is: ~a~%" insertions)
    (setf cmds (loop for cur-ptr in insertions 
		     for i from 0 to (- (length insertions) 1)
		     collecting `(setf (aref ,tmparr ,i) ,cur-ptr)) )

    ;;    (format t "Commands is: ~a~%" cmds)
    
    `(unless (,acc-name ,obj)
      (let (,tmparr)
	#-(or cmu sbcl)
	(setf ,tmparr (make-array ,howmany :element-type ',el-type))
	#+(or cmu sbcl)
	(setf ,tmparr (make-array ,howmany))
	,@cmds
	(setf (,acc-name ,obj) ,tmparr)))
    ))

(defmacro add-subelements (obj &rest attrs)
  (let* ((acc-name (concat-pnames "xml-class." "sub-Info-ptr"))
	(fun-name (concat-pnames "make-xml-subelement-" "info"))
	(sym-name (concat-pnames "+sds-" "module-name+"))
	(mod-val (symbol-value sym-name))
	;;(mod-val (symbol-value (find-symbol (format nil "~a" :+sds-module-name+) *package*)))
	(el-type (concat-pnames "" "xml-subelement-info"))
	(howmany (length attrs))
	(insertions nil)
	(cmds nil)
	(type-name nil)
	(const-name nil)
	(tmparr (gensym)))
    
    (dolist (curptr (reverse attrs))
      (if (not (and (consp curptr) (= (length curptr) 3)))
	  (error "Arguments to macro add-subelements should be on the form (type constant slot)")
	  (progn
	    (setf type-name (concat-pnames "=subelement-" (car curptr) "="))
	    (setf const-name (concat-pnames "+" mod-val "-name-" (cadr curptr) "+"))
	    ;;	(format t "Type-name is ~a~%" type-name)
	    (push `(,fun-name ,const-name ',type-name ',(caddr curptr)) insertions)
	    )))
    
    ;;    (format t "Insertions is: ~a~%" insertions)
    (setf cmds (loop for cur-ptr in insertions 
		     for i from 0 to (- (length insertions) 1)
		     collecting `(setf (aref ,tmparr ,i) ,cur-ptr)) )

    ;;    (format t "Commands is: ~a~%" cmds)
    
    `(unless (,acc-name ,obj)
      (let (,tmparr)
	;;	 (format t "Adding subelems in ~a~%" (its-name ,obj))
	#-(or cmu sbcl)
	(setf ,tmparr (make-array ,howmany :element-type ',el-type))
	#+(or cmu sbcl)
	(setf ,tmparr (make-array ,howmany))
	,@cmds
	(setf (,acc-name ,obj) ,tmparr)))
    ))

;;;(defmacro add-subelements (obj howmany &rest attrs)
;;;  (let ((acc-name (concat-pnames "xmlclass." "subInfo-ptr")))
;;;    `(unless (,acc-name ,obj)
;;;       (setf (,acc-name ,obj) (make-array ,howmany :fill-pointer 0))
;;;       ,@attrs)))


;; maybe change accesor to modname-clname.slot
(defmacro def-sds-class (name slots &optional xml-name)
  "
This is a definitive monster macro. It has two required parameters;
  name - which names the class and ends up as MODNAME-name
  slots - which should be on the form (slot1 slot2 slot3)
and it has an optional third:
  xml-name - which names the constant with the name in xml

if the third argument is given, the macro create-basics-for-class
is also invoked. 

The generated class autocreates the following info for a given SLOT
  (SLOT :accessor MODNAME-CLASSNAME-SLOT
        :initarg :SLOT
        :initform nil
        :type list)

where MODNAME and CLASSNAME is obvious

Oh and yes, the created class inherits from XML-Class in package APISPEC-XML.

The most recent addition is that all classes, and all accessors 
are exported.. very convenient..
"
  ;;  (warn "defining class ~a" name)
  
  (let* ((xmlclass-word (concat-pnames "xml-" "class"))
	 (sym-name (concat-pnames "+sds-" "module-name+"))
	 (mod-val (symbol-value sym-name))
	 #|| (find-symbol (format nil "~a" :+sds-module-name+) *package*) ||#
	
	 (full-class-name (concat-pnames mod-val "-" `,name))
	 (export-list (list full-class-name))
	 (attrinfo-name (concat-pnames "" "attr-info"))
	 (subinfo-name (concat-pnames "" "sub-info"))
	 (attrinfo-acc (concat-pnames "xml-class." "attr-info-ptr"))
	 (subinfo-acc (concat-pnames "xml-class." "sub-info-ptr"))
	 (new-slots `((,attrinfo-name :initarg ,attrinfo-name :accessor ,attrinfo-acc :initform nil :allocation :class)
		      (,subinfo-name :initarg ,subinfo-name :accessor ,subinfo-acc :initform nil :allocation :class)))
	 ;;	 (new-slots nil)
	 (basic-decl nil))

    ;;    (setf export-list (list full-class-name))
    ;;    (format t "exp is : ~a~% " export-list)
    (when `,xml-name
      (setf basic-decl `(create-basics-for-class ,name ,xml-name)))
    
    ;;    (format t "when defining class name is type ~a~%" (type-of `,name))
    
    (when (consp slots)

      ;; should check for conses with info so I can add type easily
      ;; inefficient
      (dolist (obj slots)
	(let* ((acc-name (concat-pnames full-class-name "." obj))
	       (initarg-name (concat-pnames ":" obj))
	       (new-val `((,obj :accessor ,acc-name :initarg ,initarg-name :initform nil :type list))))
	  (setf export-list (nconc export-list (list acc-name)))
	  (setf new-slots (nconc new-slots new-val)))))

    ;;(format t "~&>> Creating class ~s of ~s |~s|~%" full-class-name mod-val sym-name)
    `(eval-when (:compile-toplevel 
		 :load-toplevel
		 #+clisp :execute)
      (defclass ,full-class-name (,xmlclass-word) ,new-slots)
      ,basic-decl
      (export ',export-list))
    ))


(defmacro create-obj-constructors (&rest args)
  "this is cmu's fault"

  (let ((cnt nil))
    
    (dolist (i args)
      (let* (;;(mod-val (symbol-value (find-symbol (format nil "~a" :+sds-module-name+) *package*)))
	     (sym-name (concat-pnames "+sds-" "module-name+"))
	     (mod-val (symbol-value sym-name))
	     (full-class-name (concat-pnames mod-val "-" (if (symbolp i) i (car i))))
	     (xml-name (if (symbolp i) `,i `,(cadr i)))
	     (constr-name (concat-pnames "make-" full-class-name))
	     (constr-hash (concat-pnames "" "*constructors*"))
	     (const-name (concat-pnames "+" mod-val "-name-" `,xml-name "+")))

	;;	(push `(format t "added ~a to constr-list~%" ',i) cnt)
	(push `(setf (gethash ,const-name ,constr-hash) (function ,constr-name)) cnt)
	(push `(export ',constr-name) cnt)
	(push `(defun ,constr-name (&key parent) (make-instance ',full-class-name :parent parent)) cnt)))
    
    `(eval-when (:compile-toplevel 
		 :load-toplevel
		 #+clisp :execute)
      ,@cnt)))
