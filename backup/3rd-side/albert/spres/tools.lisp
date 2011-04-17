;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/tools.lisp - various tool-functions for spres-code
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres-impl)

  


(defun rep-table-to-list (table)
  "collects the actual objects of the table into a new list"
  (let ((retval nil))
    (maphash #'(lambda (k v)
		 (declare (ignore k))
		 (dolist (x v)
		   (push (cdr x) retval)))
	     table)
    retval))


(defun search-helper (parent kids type cur-path)
  "This function basically does the job for the generic function
search-for-types-in-sdoc-tree and relies on the methods to supply
correct arguments"
  
  (declare (ignore parent))
  
  (let ((result nil)
	(tmpres nil))
    
    (dolist (x kids)
      ;;      (format t "Comparing ~a and ~a ~%" (type-of x) type)
      (if (eq (type-of x) type)
	  (progn
	    (setq tmpres (list (cons x cur-path)))
	    (setq result (if result
			     (nconc result tmpres)
			     tmpres)))
	  (progn
	    (setq tmpres (search-for-types-in-sdoc-tree x type (append (list x) cur-path)))
	    (when tmpres
	      (setq result (if result 
			       (nconc result tmpres) 
			       tmpres))))))

    result))


(defmethod search-for-types-in-sdoc-tree (sdoc-tree type cur-path)
  (declare (ignore sdoc-tree type cur-path))
  ;;  (format t "No handler written yet for (searc-for-types-.. ~a ~a ...)~%"
  ;;	  (its-name sdoc-tree) type)
  nil)

;; integrate with helper later
(def-or-method search-for-types-in-sdoc-tree ((sdoc-tree (or sdoc:sdoc-toplevel
							     sdoc:sdoc-module
							     sdoc:sdoc-class
							     sdoc:sdoc-category)) type cur-path)
  (search-helper sdoc-tree (slot-value sdoc-tree 'sdoc::content) type cur-path))


(defun info-to-field (doc field info-name info-list)
  (let ((the-info (get-info-of-type info-list info-name)))
    (when the-info
      ;; assume just one
      (dolist (x the-info)
	(put doc "<" field ">" (car (slot-value x 'value)) "</" field ">" (eol))))))


(defun make-valid-entity (word &key (allow nil))
  "translates the word into something eatable as entity and id in
sml and label in tex.  It is returned as a _string_"
  (with-output-to-string (str)
    (loop for x across word
	  do
	  (cond ((and (consp allow) (find x allow))
		 (write-char x str))
		((find x '(#\% #\? #\/ #\* #\& #\! #\* #\$
			   #\~ #\[ #\] #\+ #\= #\@ #\, #\^
			   #\( #\) #\# #\< #\> #\Space) :test #'eql)
		 (write-string +id-word-delim+ str))
		(t
		 (write-char x str))))))



(defmethod get-newline ((doc docbook-document))
  (strcat "<big-line-break>" (string #\Newline)))

(defmethod get-simple-link ((doc docbook-document) dest desc &key hovertext)
  (cond ((stringp hovertext)
	 (setf hovertext (strcat "hovertext=\""
				 (apispec-xml:xmlify-string
				  (string-trim '(#\Space #\Tab #\Newline)
					       hovertext))
				 "\" ")))
	(t
	 (setf hovertext "")))
  
  (strcat "<link " hovertext "linkend=\"" dest "\">" desc "</link>"))

(defmethod taggify ((doc docbook-document) (type (eql :big-header)) str)
  (strcat "<really-big-header>" str "</>"))

(defmethod get-simple-anchor ((doc docbook-document) anchor-word)
  (strcat "<anchor id=\"" anchor-word "\"/>"))


(defmethod get-linked-word ((doc docbook-document) word link-type &key (desc nil))
  ;;(warn "calls get-linked-word should be replaced.. ")
  ;;  (warn "Getting linked word: ~a ~a" word link-type)
  (case link-type
    (:class (get-simple-link doc (strcat "class" +id-word-delim+ word) (if desc desc word)))
    (:method (get-simple-link doc (strcat "meth" +id-word-delim+ word) (if desc desc word)))
    (:package (get-simple-link doc (strcat "package" +id-word-delim+ word) (if desc desc word)))
    (:module (get-simple-link doc (strcat "package" +id-word-delim+ word) (if desc desc word)))
    (otherwise word))
  
  )

(defun tl-get-fields (type obj)
  (let ((info-list (slot-value obj 'info)))
    (mapcar #'caar
	    (strip-info-fields (get-info-of-type info-list type)
			       "type"))))


(defun %strip-setf (x)
  (when (> (length x) 7)
    (when (and (char-equal (schar x 0) #\()
	       (char-equal (schar x 1) #\s)
	       (char-equal (schar x 2) #\e)
	       (char-equal (schar x 3) #\t)
	       (char-equal (schar x 4) #\f)
	       (char-equal (schar x 5) #\Space)
	       (char-equal (schar x (1- (length x))) #\)))
      (return-from %strip-setf (subseq x 6 (- (length x) 1)))))
  x)

(defun tl-sort-by-name (list)
  "Returns a sorted list, based on names of objects."
  (stable-sort (copy-seq list)
	       #'(lambda (x y)
		   (string-lessp (%strip-setf x) (%strip-setf y)))
	       :key #'get-object-name))

(defun invoke-content-manager (obj doc list)
  ;; ensure correct correct-manager is chosen
  (print-content-list obj doc list))

(defmacro present-with-content-manager (obj doc list)
  `(invoke-content-manager ,obj ,doc ,list))

(defun tl-make-category (&key type name obj-list parent)
  "Creates and returns an appropriate category object."
  
  (unless name (setq name ""))
  
  (let ((cat (make-sdoc-category :parent parent))
	(n-list (list name)))

    (setf (sdoc-category.name cat) n-list
	  (sdoc-category.id cat) n-list
	  (sdoc-category.type cat) (if type (list type) nil))
	  
    (setf (sdoc-category.content cat) (reverse obj-list))
    
    cat))

(defun tl-add-to-category (cat obj)
  "Adds an object to a category."
  
  (let ((content-list (sdoc-category.content cat)))

    (if content-list
	(setf (sdoc-category.content cat)
	      (nconc (sdoc-category.content cat) (list obj)))
	(setf (sdoc-category.content cat)
	      (list obj)))
    (setf (parent-of obj) cat)
    obj))



(defun tl-categorise-duplicates (the-list &key type)
  "Makes (simple) categories of objects that match names."
  
  (let ((new-list (arrange-duplicates the-list))
	(tmp-list nil))
    
    (dolist (i new-list)
      (cond ((cdr i)
	     (push (tl-make-category :type type :name (get-object-name (car i))
				     :obj-list i)
		   tmp-list))
	    
	    ((eq (cdr i) nil)
	     (push (car i) tmp-list))))
    
    tmp-list))

  
(defmethod almost-empty-obj? ((obj sdoc-variable))

  (let* ((doc-list (slot-value obj 'doc)))
    (if doc-list
	nil
	t)))


(defmethod almost-empty-obj? ((obj sdoc-method))

  (let* ((doc-list (slot-value obj 'doc)))
    (if doc-list
	nil
	t)))


(defmethod mergable-objs? (doc first-obj second-obj)
  (declare (ignore doc first-obj second-obj))
  nil)


(defun tl-clean-for-leading-stars (str)
  "makes sure comments look sane when output.  Contains
a check on language.. the same string is returned in languages where
leading star isn't common.  Hack as needed."
  ;; hackish
  (case ?prog-lang
    (:lisp str)
    (:python str)
    (otherwise
     (with-output-to-string (out-str)
       (with-input-from-string (in-str str)
	 (loop for x = (read-line in-str nil 'eof)
	       until (eq x 'eof)
		 do
		 (let ((reduced-str (string-left-trim '(#\Space #\Tab) x)))

		   (when (plusp (length reduced-str))
		     ;;(warn "checking ~a" (schar reduced-str 0))
		     (if (eql (schar reduced-str 0) #\*)
			 (write-string (subseq reduced-str 1) out-str)
			 (write-string reduced-str out-str)))))
	 )))
    ))


(defun tl-show-location? (where)
  "checks if location should be whown for where."
  (declare (ignore where))
  (cond ((is-prog-lang? :java)
	 nil)
	(t t)))


(defun tl-print-tree (stream tree)
  "Prints the tree to the stream."

  (unless (consp tree)
    (error "The tree-argument to TL-PRINT-TREE should be of type CONS"))

  (unless (symbolp (car tree))
    (error "The first part of tree [~a] is not a symbol" (car tree)))
	 
  (let ((ptr tree))

    (write-char #\< stream)
    (format stream "~a" (car tree))

    ;; checking for attrs
    (setq ptr (cdr ptr))
	 
    (when (and ptr (car ptr))
      (cond ((consp (car ptr))
	     (let ((attr-list (car ptr)))
	       (dolist (i attr-list)
		 (assert (consp i))
		 (format stream " ~a=\"~a\"" (car i) (cdr i)))
	       ))

	    (t
	     (error "Really weird attrs ~s" (car ptr)) 
	     )))

    (when (and ptr (cdr ptr))
      (setq ptr (cdr ptr)))

    (if ptr
	(write-char #\> stream)
	(progn
	  (write-string "/>" stream)
	  (terpri stream)
	  (return-from tl-print-tree nil)))

    ;; do other stuff
    (dolist (i ptr)
      (cond ((consp i)
	     (tl-print-tree stream i))
	    (i
	     (format stream "~a" i))))

    (write-string "</" stream)
    ;; elmname
    (format stream "~a" (car tree))
    (write-char #\> stream)
    (terpri stream)))



(defun tree-put (doc tree)
  "Puts the tree to the given document."
  (let ((str (document.content doc))
	(*print-case* :downcase))
    (tl-print-tree str tree)))


(defun tl-merge-list (doc the-list)
  "Tries to merge the list appropriately.
The list should be sorted."

  (let ((result nil)
	(lastptr nil)
	(gatherer nil))

    (labels ((add-to-group (group obj)
	       (tl-add-to-category group obj))
	     (make-group (base-obj)
	       ;;(warn "$$$ we create category for ~a" base-obj)
	       (let ((guessed-type (case (type-of base-obj)
				     ('sdoc-method "method")
				     ('sdoc-variable "variable")
				     (otherwise nil))))
		 #||
		 (when (string-equal (get-object-name base-obj) "PRESENT-OBJECT")
		   (warn "MAKE CAT for P-O with ~s ~s ~s" base-obj (parent-of base-obj) (parent-of (parent-of base-obj))))
		 ||#
		 (tl-make-category :name (get-object-name base-obj)
				   :type guessed-type
				   :parent (parent-of base-obj))))
	     
	     (clean-gathering ()
	       (add-to-group gatherer lastptr)
	       (setq lastptr gatherer)
	       (setq gatherer nil)
	       ;;(warn "including ~a" (tl-get-cat-info-str lastptr))
	       )
	     )
   
      (dolist (i the-list)
	(cond (lastptr
	       (cond ((mergable-objs? doc i lastptr)
		      ;; we need a gatherer
		      (unless gatherer
			(setq gatherer (make-group lastptr)))
		    
		      (add-to-group gatherer lastptr)
		      (setq lastptr i))

		     ;; we have a lastptr and possibly a gatherer
		     (t
		      (when gatherer
			(clean-gathering)
			)

		      (push lastptr result)
		      (setq lastptr i))
		     ))
	    
	      ;; we have no lastptr
	      (t
	       (setq lastptr i))))

      (when gatherer
	(clean-gathering))
	     
      (when lastptr
	(push lastptr result)
	(setq lastptr nil))

      ;;(warn "Merge of ~s gave ~s" the-list (reverse result))
      ;; return things
      (nreverse result))))
			
			
(defun tl-get-cat-info-str (cat)
  "returns a string with info about a cat. [DEBUGGING FUN]"
  (let ((content-list (slot-value cat 'content)))
    (format nil "%Category ~a of type ~s which have members: ~a"
	    (get-object-name cat)
	    (get-string sdoc-category.type cat)
	    (mapcar #'get-object-name content-list))))



(defun tl-divide-and-order-objects (obj-list lang &key (also-sort nil))
  "Divides the object-list from settings depending on
language and returns a list of buckets."

  (declare (ignore lang))

  (let* ((last-bucket 4)
	 (buckets (make-array (1+ last-bucket) :initial-element nil))
	 (sort-nums '((sdoc-class . 0)
		      (sdoc-category . 1)
		      (sdoc-method . 2)
		      (sdoc-variable . 3))))
    
    (dolist (i obj-list)
      (let* ((the-type (type-of i))
	     (sort-order (assoc the-type sort-nums)))
	
;;	(warn "Checking ~s and get ~s" the-type sort-order)
	
	(push i (aref buckets (if sort-order (cdr sort-order) last-bucket)))))

    (loop for x across buckets
	  collecting
	  (if also-sort
	      (tl-sort-by-name x)
	      (nreverse x)))
    ))


(defun tl-get-sect-heading-from-type (type)
  "Returns a section heading based on type."

  (let ((default-value "Other Content")
	(meth-val "Methods")
	(relmeth-val "Related Methods")
	(var-val "Variables")
	(slot-val "Slots")
	(cat-val "Categories")
	(tspec-val "Type-specifiers")
	(enum-val "Enums"))

    ;;(warn "Checking ~s header for ~s ~s" type ?obj ?parent)
    (cond ((and (typep type 'sdoc-method)
		(has-spres-flag? :related-methods))
	   relmeth-val)
	  ((typep type 'sdoc-method)
	   meth-val)
	  ((typep type 'sdoc-variable)
	 
	   (if (and (is-prog-lang? :lisp) (typep ?parent 'sdoc-class))
	       slot-val
	       var-val))
	  ((typep type 'sdoc-typespec)
	   tspec-val)
	  ((typep type 'sdoc-enum)
	   enum-val)
	  
	  ((typep type 'sdoc-category)
	   (let ((cat-type (car (sdoc-category.type type))))
	     (cond ((equal cat-type "method")
		    meth-val)
		   ((equal cat-type "variable")
		    (if (and (is-prog-lang? :lisp) (typep ?parent 'sdoc-class))
			slot-val
			var-val))
		   ((equal cat-type "userspec")
		    cat-val)
		   (t
		    (error "Fell through category with type ~s" cat-type)))))
	  (t
	   (warn "fell through section-check with ~s" type)
	   default-value)
	  )))

(defun tl-get-next-section-state (cur-state)
  "Returns a cons with the next state and the name of the tag."

  (ecase cur-state
    (:reference (cons :refsect1 "refsect1"))
    (:refsect1 (cons :refsect2 "refsect2"))
    ))
	
(defun filter-away (the-list predicate)
  "works as REMOVE-IF but returns the removed values as second value"

  (let ((filtered nil)
	(normal nil))
    (dolist (i the-list)
      (if (funcall predicate i)
	  (push i filtered)
	  (push i normal)))
    (values (nreverse normal)
	    (nreverse filtered))))

  

(defmethod collect-indexable ((obj sdoc-class))

  (let* ((this-idx (make-idx-entry :name (get-object-name obj) :obj obj :parent ?parent))
	 (?parent obj)
	 (its-content (loop for i in (slot-value obj 'content)
			    collecting (collect-indexable i))))
    (cons this-idx its-content)))

(def-or-method collect-indexable ((obj (or sdoc-method sdoc-variable sdoc-enum sdoc-typespec)))

  (make-idx-entry :name (get-object-name obj) :obj obj :parent ?parent))


(defun sort-idx-list (idx-list)
  "Sorts and returns the idx-list."
  (stable-sort idx-list
	       #'string-lessp
	       :key #'idx-entry-name))

(defun tl-make-string-desc (obj)
  (etypecase obj
    (string obj)
    (sdoc-inherit (get-object-name obj))))

(defun tl-make-link-for-class (the-class)
  "Tries to make a link for a class-name or returns the name."

  
  
  (let ((the-node nil)
	(the-name nil))

    (etypecase the-class
      (string (setf the-name the-class
		    the-node (find-class-in-hierarchy ?class-hierarchy the-class)))
      (sdoc-inherit (setf the-name (get-object-name the-class))
		    (setf the-node (find-class-in-hierarchy ?class-hierarchy the-name)))
      (sdoc-class (setf the-name (get-object-name the-class))
		  (setf the-node (find-class-in-hierarchy ?class-hierarchy the-name)))
      (tree-node (setf the-name (get-object-name (tnode.class the-class))) ;; this one might fail
		 (setf the-node the-class)))
    
    ;;(warn "When trying to make a link to ~s we got ~s" the-name found-class)
    
    (if the-node
      (make-obj-link ?doc
		     (tnode.class the-node)
		     (tnode.scope the-node)
		     :only-if-exists nil)
 
      the-name)))

(defun tl-merge-two-paths (new-path base)
  "Tries to merge two paths.."

  (let ((new-dir (append (pathname-directory base)
			 (cdr (pathname-directory new-path)))))
    (make-pathname :name (pathname-name new-path)
		   :type (pathname-type new-path)
		   :directory new-dir)))

(defun %check-for-class-mod (obj class-mod)
  (when (typep obj 'sdoc-class)
    (when-bind (type-list (get-info-of-type (slot-value obj 'info) "mod"))
      (let ((presentable-type-list (mapcar #'caar (strip-info-fields type-list "type"))))
	(find class-mod presentable-type-list :test #'equal)
	))))

(defun tl-is-interface? (obj)
  "Checks if a class is an interface."
  (%check-for-class-mod obj "interface"))

(defun tl-is-struct? (obj)
  "Checks if a class is a struct."
  (%check-for-class-mod obj "struct"))

(defun update-calledby-info! (obj)
  "Grovels over the full tree and adds calledby-info."
  
  (etypecase obj
    (sdoc:sdoc-toplevel
     (let ((*scope-stack* (cons obj *scope-stack*)))
       (dolist (i (sdoc:sdoc-toplevel.content obj))
	 (update-calledby-info! i))))
    
    (sdoc:sdoc-package
     (let ((*scope-stack* (cons obj *scope-stack*)))
       (dolist (i (sdoc:sdoc-package.content obj))
	 (update-calledby-info! i))))

    (sdoc:sdoc-class
     (let ((*scope-stack* (cons obj *scope-stack*)))
       (dolist (i (sdoc:sdoc-class.content obj))
	 (update-calledby-info! i))))

    (sdoc:sdoc-method
     ;;(warn "Must check method ~s" obj)
     ;;(warn "val is ~s" (sdoc:sdoc-method.info obj))
     (dolist (i (sdoc:sdoc-method.info obj))
       (when (equal (car (sdoc:sdoc-info.type i)) "calls")
	 (let ((x (car (sdoc:sdoc-info.value i))))
	   (cond ((and (is-prog-lang? :lisp)
		       (link-to-clhs? x))
		  ;;(spres-impl::clhs-link doc x)
		  nil
		  )
		 (t
		  (assert (typep obj 'sdoc-method))
		  (assert (stringp x))
		  (let ((lookup (recursively-lookup-name obj x :method)))
		    (when (consp lookup)
		      (push (fill-info-obj (make-sdoc-info)
					   "calledby"
					   (cons obj (car *scope-stack*)) ;; will this work?
					   nil)
			    (sdoc:sdoc-method.info (car lookup)))
		      )))
		 ))
	 )))

    (sdoc:sdoc-variable nil)))

(defmethod update-parent-status! (obj parent)
  nil)

(defmethod update-parent-status! ((obj sdoc-toplevel) parent)
  (assert (eq parent nil))
  
  (setf (parent-of obj) parent)
  
  (dolist (i (slot-value obj 'content))
    (update-parent-status! i obj))
  nil)

(defmethod update-parent-status! ((obj sdoc-package) parent)
  (assert (typep parent 'sdoc-toplevel))

  (setf (parent-of obj) parent)
    
  (dolist (i (slot-value obj 'content))
    (update-parent-status! i obj))
  nil)

(defmethod update-parent-status! ((obj sdoc-class) parent)
  (unless (typep parent 'sdoc-package)
    (warn "Found class ~s in ~s" (get-object-name obj) (get-object-name parent)))
  
  (setf (parent-of obj) parent)
    
  (dolist (i (slot-value obj 'content))
    (update-parent-status! i obj))
  nil)

(defmethod update-parent-status! ((obj sdoc-method) parent)
  (unless (or (typep parent 'sdoc-package)
	      (typep parent 'sdoc-class)
	      (typep parent 'sdoc-method))
    (warn "Found method ~s in ~s" (get-object-name obj) (get-object-name parent)))
  
  (setf (parent-of obj) parent)
  
  (dolist (i (slot-value obj 'content))
    (update-parent-status! i obj))
  nil)

(defmethod update-parent-status! ((obj sdoc-variable) parent)
  (unless (or (typep parent 'sdoc-package)
	      (typep parent 'sdoc-class))
    (warn "Found variable ~s in ~s" (get-object-name obj) (get-object-name parent)))

  (setf (parent-of obj) parent)
  
  nil)

(defmethod get-enclosing-package (obj)
  (warn "Don't know enclosing package for ~s, with parent-of ~s" obj (parent-of obj))
  nil)

(defmethod get-enclosing-package ((obj sdoc-toplevel))
  nil)

(defmethod get-enclosing-package ((obj sdoc-package))
  obj)

(defmethod get-enclosing-package ((obj sdoc-category))
  (get-enclosing-package (parent-of obj)))

(defmethod get-enclosing-package ((obj sdoc-class))
  (get-enclosing-package (parent-of obj)))

(defmethod get-enclosing-package ((obj sdoc-method))
  (get-enclosing-package (parent-of obj)))

(defmethod get-enclosing-package ((obj sdoc-variable))
  (get-enclosing-package (parent-of obj)))


(defun collect-parent-path (obj)
  "Returns the parents of OBJ in a list."
  (if (parent-of obj)
      (cons (parent-of obj) (collect-parent-path (parent-of obj)))
      nil))

(defun get-sdoc-toplevel ()
  "Returns the toplevel sdoc-object."
  (let ((toplevel nil))
    (dolist (i *scope-stack*)
      (when (typep i 'sdoc-toplevel) (setf toplevel i)))
    toplevel))
