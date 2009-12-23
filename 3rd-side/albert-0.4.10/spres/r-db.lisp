;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: spres-impl -*-

#|

DESC: spres/r-db.lisp - docbook definitions for generated code
Copyright (c) 1998-2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#


(in-package :spres-impl)

(defvar *default-docbook-dtd* (albert-setting '("albert" "docbook" "dtd")))

(defun db-get-dtd ()
  "Returns a string with the chosen docbook dtd."
  *default-docbook-dtd*)

(defun (setf db-get-dtd) (dtd-spec)
  "Registers a docbook dtd for use."
  (setf *default-docbook-dtd* dtd-spec))


(defmethod make-document (dir filename (format format-docbook) lang)
  "Creates a document..."

;;  (when-verbose
;;      (format t "~&Making document [~a,~a] in ~a/~a~%" format lang dir filename))
  
  (let ((doc (make-instance 'docbook-document :directory dir :filename filename 
			    :format format :lang-words lang)))
    
    doc))


(defmethod present-document ((doc docbook-document)  &key content-prefix content-suffix)
  
;;    (when-verbose
;;	(warn "Going for presentation of ~a" doc))
    
  (let* ((out-path (tl-merge-two-paths (pathname (make-decent-filename
						 (strcat
						  (document.filename doc)
						  (get-file-extension doc))))
				       (document.directory doc)))
	 (out-file (merge-pathnames out-path)))


    (make-sure-dirs-exist out-path)
    
    #||
    (when-verbose
	(format t "Writing document [~s,~s][~s] -> ~s~%" 
		(document.directory doc) 
		(document.filename doc)
		(pathname (make-decent-filename
			   (strcat
			    (document.filename doc)
			    (get-file-extension doc))))
		out-path))
    ||#
    
    (with-open-file (out-stream out-file
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
      
      (cond ((stringp content-prefix)
	     (write-string content-prefix out-stream))
	    ((functionp content-prefix)
	     (funcall content-prefix doc out-stream))
	    (t nil))
      
      (write-string (get-output-stream-string (document.content doc))
		    out-stream)
      
      (cond ((stringp content-suffix)
	     (write-string content-suffix out-stream))
	    ((functionp content-suffix)
	     (funcall content-suffix doc out-stream))
	    (t nil))
      )


    #||
    (ignore-errors
      (unless-quiet
       (albert-info "Wrote docbook book to ~a" (namestring out-file))))
    ||#
    doc))


(defmethod clhs-link ((doc docbook-document) name)

  (let ((cl-found (link-to-clhs? name)))
    (if cl-found
	(strcat "<ulink type=\"clhs\" url=\"" *clhs-root* (car cl-found) "\">"
		;;"CL:"
		(apispec-xml:xmlify-string name) "</ulink>")
	name)))


(defmethod generate-tag ((doc docbook-document) tag end-tag?)
  (if end-tag?
      (strcat "</" tag ">")
      (strcat "<" tag ">")))

(defmethod taggify ((format docbook-document) (type (eql :bold)) str)
  ;; not really used..
  str)

(defmethod taggify ((format docbook-document) (type (eql :italic)) str)
  ;; not really used..
  str)
   

(defun db-present-doc-list (doc-list doc type &key suppress-wrapper)
  "Takes a doc-list and presents it to the given doc object"

  (flet ((present-doc-entry (the-type the-doc-objs)
	   (let ((handler (get-doc-handler the-type)))

	     ;;(warn "Passing ~s" the-doc-objs)
	     (if handler
		 ;; the handler must respond to empty content itself
		 (funcall handler doc the-doc-objs type)
		 (let* ((text-list (mapcar #'(lambda (x)
					      (get-string sdoc-doc.text x))
					  the-doc-objs))
			(good-content (some #'stringp text-list)))
		   (when good-content

		     (put doc "<varlistentry>" (eol))
		     (put doc "<term>" (get-doc-word the-type doc) "</term>" (eol))
		     (put doc "<listitem>" (eol))
		     ;;(warn "dealing with ~s -> ~s"  (get-doc-word the-type doc)
		     ;;      (get-string sdoc-info.value (car the-doc-objs)))
		     (dolist (x text-list)
		       (put doc "<simpara>" 
			    (apispec-xml:xmlify-string
			     (tl-clean-for-leading-stars x))
			    "</simpara>" (eol)))
		     (put doc "</listitem>")
		     (put doc "</varlistentry>" (eol)))))
	     )))
  
    ;; fix sorted slution later if needed
    (when doc-list
      (unless suppress-wrapper
	(put doc "<variablelist>" (eol)))

      (let ((entries (make-hash-table :test #'equal)))
	(dolist (x doc-list)
	  (push x (gethash (get-string sdoc-doc.type x) entries)))

	(maphash #'present-doc-entry entries))
    
      (unless suppress-wrapper
	(put doc "</variablelist>" (eol)))
    
      )))


(defun db-present-info-list (info-list doc type &key suppress-wrapper)
  "Takes a doc-list and presents it to the given doc object"

  (flet ((present-info-entry (the-type the-info-objs)
	   (let ((handler (get-info-handler the-type)))
	     ;;(warn "Passing ~s" the-doc-objs)
	     (if handler
		 (funcall handler doc the-info-objs type)
		 (progn
		   (put doc "<varlistentry>" (eol))
		   (put doc "<term>" (get-doc-word the-type doc) "</term>" (eol))
		   (put doc "<listitem>" (eol))

		   (dolist (x the-info-objs)
		     (put doc "<simpara>" 
			  (apispec-xml:xmlify-string
			   (tl-clean-for-leading-stars (get-string sdoc-info.value x))) 
			  "</simpara>" (eol)))
		   (put doc "</listitem>")
		   (put doc "</varlistentry>" (eol)))))
	   ))
    ;; fix sorted slution later if needed
    (when info-list
      ;; (warn "Presenting info-list ~a" info-list)
      (unless suppress-wrapper
	(put doc "<variablelist>" (eol)))

      (let ((entries (make-hash-table :test #'equal)))
	(dolist (x info-list)
	  (push x (gethash (car (sdoc-info.type x)) entries)))

	(maphash #'present-info-entry entries))
    
      (unless suppress-wrapper
	(put doc "</variablelist>" (eol)))
	 
      )))


(defmethod handle-param-doc ((doc docbook-document) doc-list type)
  
  (declare (ignore type))
  
  (let ((the-type (get-string sdoc-doc.type (car doc-list))))
    (put doc "<varlistentry>" (eol))
    (put doc "<term>" (get-doc-word the-type doc) "</term>" (eol))
    (put doc "<listitem>" (eol))
    (dolist (x doc-list)
      (let ((full-text (tl-clean-for-leading-stars (get-string sdoc-doc.text x)))
	    (first-word nil)
	    (pos 0)
	    (rest nil))
	(setq pos (position #\Space full-text))
	(setq first-word (subseq full-text 0 pos))
	(setq rest (subseq full-text (1+ pos)))
      
	(put doc "<simpara>" "<emphasis>" first-word "</emphasis>  -  " 
	     (apispec-xml:xmlify-string rest)
	     "</simpara>" (eol))))
    (put doc "</listitem>")
    (put doc "</varlistentry>" (eol))))


(defmethod handle-see-doc ((doc docbook-document) doc-list type)
  
  (dolist (doc-obj doc-list)
  
    (let ((text (get-string sdoc-doc.text doc-obj))
	  (the-type (get-string sdoc-doc.type doc-obj)))
      (cond
       ((eq :class type)
	(put doc "<refsect2>" (eol)
	     "<title>" (get-doc-word the-type doc) "</title>" (eol))
	(put doc "<simpara>" (apispec-xml:xmlify-string (tl-clean-for-leading-stars text)) "</simpara>" (eol))
	(put doc "</refsect2>" (eol)))
       
       ((or (eq :method type) (eq :variable type))
	(put doc "<varlistentry>" (eol))
	(put doc "<term>" (get-doc-word the-type doc) "</term>" (eol))
	(put doc "<listitem><simpara>add link to '" text "' here</simpara></listitem>" (eol))
	(put doc "</varlistentry>" (eol)))))))


(defmethod handle-calls-info ((doc docbook-document) doc-list type)

  (declare (ignore type))
  #||
  (warn "Type is ~s" doc-list)
  (cond ((and (equal type "calls") (not (albert-setting '("albert" "presentation" "funcallable" "calls"))))
	 (return-from handle-calls-info nil))
	((and (equal type "calledby") (not (albert-setting '("albert" "presentation" "funcallable" "calledby"))))
  (return-from handle-calls-info nil)))
  ||#
  (let ((the-type (get-string sdoc-info.type (car doc-list))))
    (put doc "<varlistentry>" (eol))
    (put doc "<term>" (get-doc-word the-type doc) "</term>" (eol))
    (put doc "<listitem>" (eol))
    
    (let* ((call-strings (loop for x in doc-list
			       collecting
			       (tl-clean-for-leading-stars (get-string sdoc-info.value x))))
	   
	   (call-list (stable-sort (remove-duplicates call-strings :test #'equal) #'string-lessp
				   :key #'(lambda (x) ;; hackish
					    (cond ((stringp x)
						   x)
						  ((and (consp x) (typep (car x) 'sdoc:sdoc-method))
						   (get-object-name (car x)))
						  (t
						   (error "Unknown value in call-list: ~s" x))))))

	   (len (length call-list)))

      
      (setq call-list (loop for x in call-list
			    collecting
			    (cond ((and (is-prog-lang? :lisp)
					(link-to-clhs? x))
				   (clhs-link doc x))
				  ;; sometimes we get pointers directly..
				  ((and (consp x) (typep (car x) 'sdoc:sdoc-method))
				   (make-obj-link doc (car x) (cdr x)))
				  (t
				   
				   ;;(when (string-equal x "SDOC-INHERIT.INFO")
				   ;;  (warn "Going to check ~s ~s" ?obj x))
				   
				   (let ((lookup2 (recursively-lookup-name ?obj x :method)))
				     (assert (typep ?obj 'sdoc-method))
				     (assert (stringp x))
				     
				     #||
				     (assert (or (eq nil lookup2)
						 (and (consp lookup2)
						      (typep (car lookup2) 'apispec-xml:xml-class)
						      (typep (cdr lookup2) 'apispec-xml:xml-class))))
				     ||#
				     #||
				     (unless lookup2
				       (warn ">> Failed for ~s" x))
				     ||#
				     #||
				     ;; ultraevil hack
				     (while (or (typep (cdr lookup2) 'sdoc-category)
						(typep (cdr lookup2) 'sdoc-method))
				       ;;(warn "HIT ~s -> ~s" (cdr lookup2) (collect-parent-path (cdr lookup2)))
				       (setf (cdr lookup2) (parent-of (cdr lookup2))))
				     ||#
				     #||
				     (when (string-equal x "SDOC-INHERIT.INFO")
				       (warn "Got back ~s + ~s [~s + ~s]"
					     (car lookup2) (cdr lookup2)
					     (get-object-name (car lookup2))
					     (get-object-name (cdr lookup2))))
				     ||#
				     
				     (if (consp lookup2)
					 (make-obj-link doc (car lookup2) (cdr lookup2)
							:desc x)
					 (apispec-xml:xmlify-string x))))
				  )))
      
      ;;(warn "from ~s to ~s" call-strings call-list)
      
      (cond ((>= len (albert-setting '("albert" "presentation" "funcallable" "calls-num")))
	     (db-print-table doc call-list :xmlify nil))

	    ;; smaller lists
	    (t

	     (tree-put doc `(:simpara nil
			     ,(list-to-sep-string call-list :and-word (get-word "and" doc))))
	     )))


    
    (put doc "</listitem>")
    (put doc "</varlistentry>" (eol))
    ))

(defun db-print-table (doc string-list &key title (columns 3) (xmlify t))
  "Prints the string-list as a table."

  (declare (ignore title))
  
  (let ((y 0)
	(end-col (1- columns)))
  
    (put doc "<informaltable frame=\"all\" shortentry=\"0\">" (eol)) 
    ;;  (put doc "<title>" (if title title "") "</title>" (eol)) 
    (put doc "<tgroup cols=\"" columns "\" colsep=\"1\" rowsep=\"1\"><tbody>" (eol))
  

    (dolist (x string-list)
      (when (= 0 (mod y columns))
	(put doc "<row>"))
      
      (put doc "<entry>" (if xmlify (apispec-xml:xmlify-string x) x) "</entry>")
      
      (when (= end-col (mod y columns))
	(put doc "</row>"))
      (incf y))
    
    (unless (= 0 (mod y columns))
      (put doc "</row>")))
  
  (put doc "</tbody></tgroup></informaltable>" (eol))) 


(defun db-print-location (loc-list doc type &key suppress-wrapper)
  "Takes a doc-list and presents it to the given doc object"

  (declare (ignore type))
  
  (when loc-list
    (unless suppress-wrapper
      (put doc "<variablelist>" (eol)))

    ;; rewrite to tree-put?
    (put doc "<varlistentry>" (eol))
    (put doc "<term>" (get-doc-word "Location" doc) "</term>" (eol))
    (dolist (i loc-list)
      (let ((val (etypecase i
		   (string i)
		   (sdoc-location (get-string sdoc-location.file i)))))
	(put doc "<listitem><simpara>" val "</simpara></listitem>" (eol))))
  
    (put doc "</varlistentry>" (eol))

    
    (unless suppress-wrapper
      (put doc "</variablelist>" (eol)))
    ))

(defun possibly-cvs-link (loc-obj)
  "Returns either a link to a cvs tree or the loc-obj."
  (let ((view-url (albert-setting '("albert" "docbook" "cvs-viewurl"))))
    (when (and (stringp view-url)
	       (plusp (length view-url))
	       (typep loc-obj 'sdoc-location))
      (let ((val (car (sdoc-location.file loc-obj)))
	    (ver (setting-or-default '("albert" "docbook" "cvs-tag") "HEAD")))
	(return-from possibly-cvs-link
	  (strcat "<ulink type=\"cvs\" url=\"" view-url val "?rev=" ver "\">"
		  (apispec-xml:xmlify-string val) "</ulink>"))))
    loc-obj))


(defun db-assoc-table-as-tree (doc obj assoc-list &key (style :table) (title nil))
  "Returns an association-list back as an appropriate DB-tree.
Known styles are :variablelist and :table"
  
  (declare (ignore obj))
  
  (unless assoc-list
    (warn "db-assoc-table-as-tree needs a proper alist.")
    (return-from db-assoc-table-as-tree nil))

  (ecase style
    (:variablelist
     `(:variablelist nil
       ,@(mapcar #'(lambda (x)
		     `(:varlistentry nil
			 (:term nil ,(get-word (car x) doc))
		       (:listitem nil
			(:simpara nil
			 ,(cdr x)))))
		 assoc-list)))
    
    (:table
     (let ((table-type (if title :table :informaltable)))
       `(,table-type (;;(:frame . "all")
		      ;;(:style . "classfacts")
		      (:role . "classfacts")
		      (:frame . "sides")
		      (:shortentry . "0"))
	 ,(if title `(:title nil ,title) nil) 
	 (:tgroup ((:cols . "2")
		   (:colsep . "3")
		   (:rowsep . "3"))
	  (:tbody nil
	   ,@(mapcar #'(lambda (x)
		       `(:row nil
			 (:entry nil ,(get-word (car x) doc))
			 (:entry nil ,(cdr x))))
		     assoc-list))
	))))

     ))


(defun db-get-id-prefix (obj)
  "Tries to create an id for an object.  returns a string."

  (etypecase obj
    (sdoc-class "class")
    (sdoc-method "meth")
    (sdoc-variable "var")
    (sdoc-package "package")
    (sdoc-module "module")
    (sdoc-enum "enum")
    (sdoc-typespec "tspec")
    ))


(defmethod make-obj-id ((doc docbook-document) obj parent &key (prefix nil) (name nil) (allow-cached nil))
  "Returns an id for an obj and it's parent."

  (labels ((write-id-to-stream (s)
	   (write-string (if prefix
			     prefix
			     (db-get-id-prefix obj))
			 s)
	   (write-string +id-word-delim+ s)
	   
	   (when (and parent
		      (not (typep parent 'sdoc-toplevel)))
	     (write-string (make-valid-entity (get-object-name parent))
			   s)
	     (write-string +id-word-delim+ s))
	   
	   (write-string (make-valid-entity (if name
						name
						(get-object-name obj)))
			 s))
	   
	   (get-id (allow-cached?)
	     (let ((quick-check nil))
	       (when allow-cached?
		 (setf quick-check (check-for-id obj)))
	       (if quick-check 
		   quick-check
		   (let ((new-id (with-output-to-string (s)
				   (write-id-to-stream s))))
		     ;; a bit slower, but should be safe, hopefully
		     ;;(register-created-id obj new-id)
		     new-id)))))
    
    (get-id allow-cached)))


;; hacky
(defmethod make-obj-id ((doc docbook-document) (obj sdoc-method) (parent sdoc-category)
			&key (prefix nil) (name nil) (allow-cached nil))
  (let ((use-parent (parent-of parent))
	(val nil))
    (while (or (typep use-parent 'sdoc-method)
	       (typep use-parent 'sdoc-category))
      (setf use-parent (parent-of use-parent)))

    (setf val (call-next-method doc obj use-parent :prefix prefix :name name :allow-cached allow-cached))
    ;;(warn "Making id for category ~s -> ~s -> ~s" obj use-parent val)
    val))

(defun get-hover-desc (doc obj)
  ;; this code must check what kind of data it really is!!!
  (etypecase obj
    (sdoc:sdoc-method (get-method-signature doc obj :linked nil))
    (sdoc:sdoc-class  (get-class-signature doc obj :linked nil))
    (sdoc:sdoc-package (strcat "(defpackage " (get-object-name obj) " ...)"))
    (sdoc:sdoc-variable (get-variable-signature doc obj :linked nil))
    ))

(defmethod make-obj-link ((doc docbook-document) obj parent &key (desc nil) (only-if-exists nil))
  ;;(warn "Obj link to ~s with desc ~s" obj desc)
  (let ((quick-check (check-for-id obj))
	(link-str nil)
	(hovertext (get-hover-desc doc obj))
	(the-desc (if desc desc (get-object-name obj))))

    ;; holds because make-obj-id doesn't register 
    (assert (eq quick-check nil))
    (if (or quick-check only-if-exists)
	(setf link-str quick-check)
	(setf link-str (make-obj-id doc obj parent)))

    (if link-str 
	(get-simple-link doc link-str the-desc :hovertext hovertext)
	the-desc)))

#||
(defmethod make-obj-link ((doc docbook-document) obj (parent sdoc-category) &key (desc nil) (only-if-exists nil))
  (let ((val (call-next-method)))
    (warn "Making link for category ~s -> ~s -> ~s" obj (collect-parent-path obj) parent)
    val))
||#

(defmethod mergable-objs? ((doc docbook-document) first-obj second-obj)
  (declare (ignore first-obj second-obj))
  nil)

(defmethod mergable-objs? ((doc docbook-document)
			   (first-obj sdoc-variable)
			   (second-obj sdoc-variable))
  
  (if (and (almost-empty-obj? first-obj)
	   (almost-empty-obj? second-obj))
      t
      nil))

(defmethod mergable-objs? ((doc docbook-document)
			   (first-obj sdoc-method)
			   (second-obj sdoc-method))

  (cond ((typep ?parent 'sdoc-method) nil)
	(t
	 (let ((fname (get-object-name first-obj))
	       (sname (get-object-name second-obj)))
	   
	   (string= fname sname)
	   ))))

(defun db-insert-index (doc from-list columns &key (only-link-existing t))
  "Inserts an index gathered from FROM-LIST and presents it in
COLUMNS columns."

  (let* ((the-index (loop for i in from-list
			 collecting (collect-indexable i)))
	(flat-index (flatten the-index)))

    (when flat-index
      ;; (warn "dumping ~s" flat-index)
      (put doc "<simplelist columns=\"" (format nil "~a" columns) "\">" (eol))
      (dolist (i (sort-idx-list flat-index))
	(put doc "<member>")
	
	(put doc (make-obj-link doc
				(idx-entry-obj i)
				(idx-entry-parent i)
				:only-if-exists only-link-existing))
	(put doc "</member>" (eol)))
	  
      (put doc "</simplelist>" (eol))
      )
  
  ))

(defun db-present-class-details (doc obj)
  "Presents class-details in a refsect in doc."

  	      
  (tree-put doc `(:refsect1 nil
		  (:title ((:class . "contenttitle"))
		   ,(get-word (if (tl-is-struct? obj) "Struct details" "Class details") doc))
		  (:programlisting nil ,(get-class-signature doc obj))
		  ;;(:programlisting nil ,(draw-hierarchy doc obj ?class-hierarchy))
		  ,(db-assoc-table-as-tree doc obj
					   (get-class-facts doc obj)
					   :style :table
					   ;;:title (get-word "Class details" doc)
					   ))))

(defun is-variable-a-constant? (obj)
  (let ((mod-info (get-info-of-type (slot-value obj 'info) "mod")))
    (dolist (inf mod-info)
      (when (string-equal (car (sdoc-info.value inf)) "constant")
	(return-from is-variable-a-constant? t)))
    nil))
				
(defmethod present-objs-in-package (doc package objs (kind (eql :vars)) (format (eql :refsect1)))
  (when objs
    (let ((const-count 0)
	  (real-count 0))
      
      (dolist (obj objs)
	(when (is-variable-a-constant? obj) (incf const-count)))
      
      (setf real-count (- (length objs) const-count))
      
      ;;(warn "~s has ~s vars but ~s constants" (get-object-name package) (length objs) const-count))

      ;; ok, this is cheeky, we make separate var/const pages if they both
      ;; breach the separatepage limit

      (cond ((and (>= real-count (albert-setting '("albert" "presentation" "variables" "separatepage")))
		  (>= const-count (albert-setting '("albert" "presentation" "variables" "separatepage"))))
	     ;; ok we need to filter
	     (let ((vars '())
		   (consts '()))
	       (loop for obj in objs do
		     (if (is-variable-a-constant? obj)
			 (push obj consts)
			 (push obj vars)))

	       ;; consts first eh?
	       (put doc "   <refsect1>" (eol)
		    "    <title>" (get-word "Constants" doc) "</title>" (eol))
	       (present-with-content-manager package doc (nreverse consts))
	       (put doc "   </refsect1>" (eol))

	       (put doc "   <refsect1>" (eol)
		    "    <title>" (get-word "Variables" doc) "</title>" (eol))
	       (present-with-content-manager package doc (nreverse vars))
	       (put doc "   </refsect1>" (eol))
	       ))
	    
	    ;; we just use one page afterall
	    (t
	     (let ((word (if (plusp const-count)
			     "Variables and Constants"
			     "Variables")))
	       (put doc "   <refsect1>" (eol)
		  "    <title>" (get-word word doc) "</title>" (eol))
	       (present-with-content-manager package doc objs)
	       (put doc "   </refsect1>" (eol))))
	    ))))


(defmethod present-objs-in-package (doc package objs (kind (eql :general)) (format (eql :refsect1)))
  (when objs
    (put doc "   <refsect1>" (eol)
	 "    <title>" (get-word "Package Content" doc) "</title>" (eol))
    (present-with-content-manager package doc objs)
    (put doc "   </refsect1>" (eol))))

(defmethod present-book-header ((doc docbook-document) stream)

  (put stream "<?xml version='1.0'?>" (eol))
  (put stream "<!DOCTYPE book PUBLIC \"-//Norman Walsh//DTD DocBk XML V3.1.7//EN\"
               \"" (db-get-dtd) "\" " )
  
  (when (and ?file-table (> (hash-table-count ?file-table) 0))
    
    (put stream "[" (eol))
    
    (loop for val being the hash-values of ?file-table
	  for fname = (file-info-to-fname val)
	  do
	  (put stream
	       "<!ENTITY "
	       "fileX" (make-valid-entity fname) " "
	       "SYSTEM \"" fname (get-file-extension doc)
	       "\">" (eol)))
    (put stream "]"))
  
  (put stream ">" (eol))
  
  (put stream "<book>" (eol))
    
  doc)


(defmethod present-book-footer ((doc docbook-document) actual-stream)
  (format actual-stream "~&</book>~%")
  doc)
