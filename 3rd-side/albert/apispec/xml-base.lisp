;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: APISPEC-XML -*-

#|

DESC: apispec/xml-base.lisp - takes care of low-level xml issues
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :apispec-xml)

#+xml-sexp-reader
(defvar *loc-sym-table* nil)

(defvar *indents* (make-array 15) 
  "a simple vector list of strings with indents")

;; we pre-allocate the indents
(dotimes (i 15)
  (setf (svref *indents* i) (make-string (* i 2) :initial-element #\Space)))

(declaim (type simple-vector *indents*))


(defvar *space-str* (string #\Space) 
  "this is just a one-character string with the space")
(defvar *dquote-str* (string #\") 
  "this is just a one-character string with the quote")
(defvar *equal-str* (string #\=) 
  "this is just a one-character string with the equal sign")

(declaim (type simple-base-string *space-str* *dquote-str* *equal-str*))


(defmethod-with-warn verify-object-actual (obj context when))

;;; Constructors

(declaim (inline loc-make-xml-attr-info loc-make-xml-subelement-info make-xml-tool))
;;(declaim (inline subelement-compare attr-compare))

(defun make-xml-attr-info (name type slot-name)
  "Returns an object of type XML-Attr-Info"
  (%make-attr-info (intern name :xml-names) 
		   type 
		   slot-name))

(defun make-xml-subelement-info (name itsType ptr)
  "Returns an object of type XML-SubElement-Info"
  (%make-sub-info (intern name :xml-names)
		  itsType 
		  ptr))


(defun make-xml-tool (factory)
  "Returns an object of type XML-Tool initialised with necessary factory"
  (make-instance 'xml-tool :factory factory))

;; helpful comparisons..
(defmacro subelement-compare (name sub-obj)
  `(if (string= ,name (xml-subelement-info-name ,sub-obj))
    ,sub-obj
    nil))


(defmacro attr-compare (name attr-obj)
  `(if (string= ,name (xml-attr-info-name ,attr-obj)) 
    ,attr-obj
    nil))


(defmacro get-indent (howmany)
  "This macro expands into code which returns a SIMPLE-BASE-STRING
with space indenting"
  `(the simple-base-string (svref *indents* ,howmany)))


;; methods

;;; maybe add a flet
(defmethod init-attributes ((xmlobj xml-class) attrs)
  "This one should be cleaned up some day but I guess it works"
  
  (flet ((my-attr-find (elm the-vec)
	   ;; (declare (optimize (safety 0) (speed 3)))
	   (loop for i of-type xml-attr-info across the-vec
	       do (when (attr-compare elm i)
		    (return-from my-attr-find i)))
	   nil))

    ;; ugly way to reduce use of slot-value
    (when attrs
      (let ((loc-attr-info (slot-value xmlobj 'attr-info)))
	(when loc-attr-info
	  (let ((elm nil)
		(where nil))

	    (dolist (cur-attr attrs)
	      (setq elm (my-attr-find #+xml-sexp-reader 
				      (symbol-name (svref *loc-sym-table*
							  (car cur-attr)))
				      #-xml-sexp-reader 
				      (car cur-attr)
				      loc-attr-info))
	      ;; fix later
	      (when elm
		
		(setq where (slot-value xmlobj (xml-attr-info-ptr elm)))
		
		(if (consp where)
		    (setf (slot-value xmlobj (xml-attr-info-ptr elm)) 
		      (nconc where (list (cdr cur-attr))))
		  (setf (slot-value xmlobj (xml-attr-info-ptr elm)) 
		    (list (cdr cur-attr)))))
	      )))
	))

    nil))

(defmethod element-start ((xmlobj xml-class) name attrs xtool)
  (flet ((my-sub-find (elm the-vec)
	   ;; (declare (optimize (safety 0) (speed 3)))
	   (loop for i of-type xml-subelement-info across the-vec
		 do (when (subelement-compare elm i)
		      (return-from my-sub-find i)))
	   nil))

    (let ((elm (my-sub-find name (slot-value xmlobj 'sub-info))))
      (when elm
	
	(let ((its-type (xml-subelement-info-the-type elm))
	      (ptr nil)
	      (the-acc (xml-subelement-info-ptr elm))
	      (cur-val nil))
	  ;;	  (warn "elm is ~a of type ~a" elm its-type)
	  (case its-type
	    
	    ((=subelement-ptr= =subelement-ptrlist=)
	     (setq ptr (produce-xml-object (xml-tool.factory xtool) name))
	     (when ptr
	       (init-attributes ptr attrs)
	       (setq cur-val (slot-value xmlobj the-acc))

	       (if cur-val
		   (setf (slot-value xmlobj the-acc) (nconc cur-val (list ptr)))
		   (setf (slot-value xmlobj the-acc) (list ptr)))))

	    
	    ((=subelement-string= =subelement-stringlist=)
	     (setf (xml-tool.want-content xtool) t)
	     (setf (xml-tool.who-content xtool) name))
	    
	    (otherwise 
	     (error "Unknown type ~s (~s) for subelement ~s in ~s~%" 
		    its-type (symbolp its-type) name (get-element-name xmlobj))))
	  
	  ptr))
      ))
  )


(defmethod element-end ((xmlobj xml-class) name xtool)
  "Returns a boolean value [nil/t]"
  ;;  (format t "Ending element ~a -> ~a ~%" name (string-equal name (get-element-name xmlobj)))
  (if (string-equal name (get-element-name xmlobj))
      t
    (setf (xml-tool.want-content xtool) nil)))


(defmethod element-content ((xmlobj xml-class) content xtool)
  (flet ((my-sub-find (elm the-vec)
	   ;;		      (declare (optimize (safety 0) (speed 3)))
	   (loop for i of-type xml-subelement-info across the-vec
		 do (when (subelement-compare elm i)
		      (return-from my-sub-find i)))
	   nil))

    (let ((elm (my-sub-find (xml-tool.who-content xtool) (slot-value xmlobj 'sub-info))))
      
      (when elm
	(let ((where (slot-value xmlobj (xml-subelement-info-ptr elm))))
	  ;; (format t "Actually adding {~a}~%" content)
	  (if where
	      (setf (slot-value xmlobj (xml-subelement-info-ptr elm)) (nconc where (list content)))
	      (setf (slot-value xmlobj (xml-subelement-info-ptr elm)) (list content)))
	  ;; (format t "Now have ~a~%" (slot-value xmlobj (xml-subelement-info-ptr elm)))
	  ))
      )))


;;#+optim-print
;;(warn "Using optimised printing..")
;;#-optim-print
;;(warn "Not using optimised printing..")

#-optim-print
(declaim (inline print-attr-if-len))
#-optim-print
(declaim (ftype (function (t t) simple-base-string) print-attr-if-len))
#-optim-print
(defun print-attr-if-len (xmlobj attr)

  (declare (optimize (safety 0) (speed 3)))
  
  (let ((the-list (slot-value xmlobj (xml-attr-info-ptr attr))))
    
    (if (consp the-list)
	(strcat
	 *space-str*
	 (the simple-base-string (symbol-name (xml-attr-info-name attr))) 
	 (the simple-base-string "=\"")
	 ;;	*equal-str*
	 ;;	*dquote-str*
	 (xmlify-string (car the-list)) 
	 *dquote-str*)
	"")))

;; this one is _hell_
#-optim-print
(defmethod print-as-xml ((xmlobj xml-class) stream xtool)
  ;;  (format t "Print as xml..~%")
  (let* ((indentlevel (incf (xml-tool.indent xtool)))
	 (attrs (slot-value xmlobj 'attr-info))
	 (element-name (get-element-name xmlobj))
	 (subs  (slot-value xmlobj 'sub-info))
	 (sublen (length subs)))
    
    (declare (type simple-base-string element-name))
    (declare (type fixnum indentlevel sublen))
    
    (let ((tmp-coll (strcat (the simple-base-string (get-indent indentlevel)) "<" element-name)))
      (declare (type simple-base-string tmp-coll))

      ;; do attrs (20ish lines -> 2 lines)
      ;; -> increased to 4 but major performance increase
      (when attrs
	(loop for cur-attr of-type xml-attr-info across (the simple-vector attrs) 
	      do (setq tmp-coll (strcat tmp-coll (print-attr-if-len xmlobj cur-attr)))))
      
      ;; write out so far
      (write-string tmp-coll stream))
    
    
    (if (= sublen 0)
	(progn
	  ;;	(format stream "/>~%")		; we're xml
	  (write-string "/>" stream)
	  (terpri stream))
      
					; else ...
	(progn
	  ;; do subelements
	  ;;	(print stream ">~%")
	  ;;	(format stream ">~%")
	  (write-char #\> stream)
	  (terpri stream)
	
	  (incf (xml-tool.indent xtool))
	
	  (let ((lastptr nil)
		(ptr nil)
		(its-type nil)
		(its-name nil))
	  
	    (loop for curobj across subs do
		  (setq lastptr ptr)
		  (setq ptr (xml-subelement-info-ptr curobj))
		  ;;  (format t "We're kicking butt on subelementent ~a ~a ~a~%" (subelement-info-name curobj) ptr lastptr)
		
		  (unless (eq ptr lastptr) ; want to avoid iterating over same element
		  
		    (setq its-type (xml-subelement-info-the-type curobj))
		    (setq its-name (symbol-name (xml-subelement-info-name curobj)))
		  
		    (case its-type
		      ((=subelement-string= =subelement-stringlist=)
		       (let ((tag-end (strcat its-name ">"))
			     (cur-indent (get-indent (xml-tool.indent xtool)))
			     (tmp-coll ""))
			 (declare (type simple-base-string tag-end cur-indent tmp-coll))
			 (dolist (my-iter (slot-value xmlobj ptr))
			   ;;	  (print-indent (xml-tool.indent xtool) stream)
			   (setq tmp-coll (strcat tmp-coll
						  cur-indent
						  "<" tag-end
						  (xmlify-string my-iter)
						  "</" tag-end)))
			 (write-line tmp-coll stream)))
		      ;; (format stream "~a<~a>~a</~a>~%" (get-indent (xml-tool.indent xtool)) its-name my-iter its-name)))
		    
		      ((=subelement-ptr= =subelement-ptrlist=)
					;		 (format t "Ptr is ~a and val is ~a~%" ptr (funcall ptr xmlobj))
		       (dolist (some-obj (slot-value xmlobj ptr))
			 (print-as-xml some-obj stream xtool)))
		      (otherwise
		       (warn "Unknown type: ~a" its-type)))
		  
		    ))			; end unless and dolist
	  
	    ;; time to wrap it up
	    (decf (xml-tool.indent xtool))
	  
	    (when (> sublen 0)
	      ;;	    (print-indent (xml-tool.indent xtool) stream)
	      (write-line (strcat (the simple-base-string (get-indent (xml-tool.indent xtool))) "</" element-name ">") stream)
	      )
	  
	    )))
    
    (decf (xml-tool.indent xtool))
    
    ))

#+optim-print
(defvar *external-format-mapping*
    ;; it was char-code-limit
    (let ((array (make-array 256 :initial-element nil)))
      (loop for code below (length array) do
;;	    (warn "checking ~a ~a" char-code-limit code)
	    (setf (svref array code) (string (code-char code))))
      (setf (svref array (char-code #\&)) "&amp;")
      (setf (svref array (char-code #\<)) "&lt;")
      (setf (svref array (char-code #\>)) "&gt;")
      (setf (svref array (char-code #\")) "&quot;")

      array)
  "A mapping from character (via char-code) to object to produce on output.  Toy version.")

#+optim-print
(declaim (type simple-vector *external-format-mapping*))

#+optim-print
(defun write-string-with-mapping (string stream)
  "Write string to stream using mappings in *external-format-mapping*.  Toy version."

  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (declare (type simple-base-string string))
  
  (loop with mapping = *external-format-mapping*
	for character of-type base-char across string do
	(write-string (the simple-base-string (svref (the simple-vector mapping) 
						     (char-code character))) stream))
  (values))


#+optim-print
(defmethod print-as-xml ((xmlobj xml-class) stream xtool)
  ;;  (format t "Print as xml..~%")
  (let* ((indentlevel (incf (xml-tool.indent xtool)))
	 (attrs (slot-value xmlobj 'attr-info))
	 (element-name (get-element-name xmlobj))
	 (subs  (slot-value xmlobj 'sub-info))
	 (sublen (length subs)))

    (declare (optimize (safety 0) (speed 3) (debug 0)))
    (declare (type simple-base-string element-name))
    (declare (type fixnum indentlevel sublen))

;;    (break)
    
    ;; whip out start and any attrs
    (write-string (get-indent indentlevel) stream)
    (write-char #\< stream)
    (write-string element-name stream)
    (when attrs
      (loop for cur-attr of-type xml-attr-info across (the simple-vector attrs) 
	  do (let ((the-list (slot-value xmlobj (xml-attr-info-ptr cur-attr))))
	       (when (consp the-list)
		 (write-char #\Space stream)
		 (write-string (the simple-base-string (symbol-name (xml-attr-info-name cur-attr))) stream)
		 (write-string "=\"" stream)
		 (write-string-with-mapping (the simple-base-string (car the-list)) stream)
		 (write-char #\" stream)))))
    

    (cond 
     ((= sublen 0)
      (write-string "/>" stream)
      (terpri stream))

     ;; otherwise
     (t
      ;; do subelements
      ;;	(print stream ">~%")
      ;;	(format stream ">~%")
      (write-char #\> stream)
      (terpri stream)
      
      (incf (xml-tool.indent xtool))
      
      (let ((lastptr nil)
	    (ptr nil)
	    (its-type nil)
	    (its-name ""))
	
	(declare (type simple-base-string its-name))
	
	(loop for curobj across subs do
	      (setq lastptr ptr)
	      (setq ptr (xml-subelement-info-ptr curobj))
	      ;;  (format t "We're kicking butt on subelementent ~a ~a ~a~%" (subelement-info-name curobj) ptr lastptr)
	      
	      (unless (eq ptr lastptr)	; want to avoid iterating over same element
		
		(setq its-type (xml-subelement-info-the-type curobj))
		(setq its-name (symbol-name (xml-subelement-info-name curobj)))
		
		(case its-type
		  ((=subelement-string= =subelement-stringlist=)
		   (let (;;(tag-end (strcat its-name ">"))
			 (cur-indent (get-indent (xml-tool.indent xtool))))
		     
		     (declare (type simple-base-string cur-indent))
		     
		     (dolist (my-iter (slot-value xmlobj ptr))
		       (write-string cur-indent stream)
		       (write-char #\< stream)
		       (write-string its-name stream)
		       (write-char #\> stream)
		       (write-string-with-mapping my-iter stream)
		       (write-string "</" stream)
		       (write-string its-name stream)
		       (write-char #\> stream))
		     (terpri stream)))

		  ;; (format stream "~a<~a>~a</~a>~%" (get-indent (xml-tool.indent xtool)) its-name my-iter its-name)))
		  
		  ((=subelement-ptr= =subelement-ptrlist=)
		;; (format t "Ptr is ~a and val is ~a~%" ptr (funcall ptr xmlobj))
		   (dolist (some-obj (slot-value xmlobj ptr))
		     (print-as-xml some-obj stream xtool)))
		  
		  (otherwise
		   (warn "Unknown type: ~a" its-type)))
		
		))			; end unless and dolist
	
	;; time to wrap it up
	(decf (xml-tool.indent xtool))
	
	(when (> sublen 0)
	  (write-string (get-indent (xml-tool.indent xtool)) stream)
	  (write-string "</" stream)
	  (write-string element-name stream)
	  (write-char #\> stream)
	  (terpri stream))
	
	)))

    
    (decf (xml-tool.indent xtool))
    
    ))


(defmethod produce-xml-object ((factory xml-factory) classname)
  (error "Wrong producer..can't produce ~a" classname)
  nil)


(defmethod parse-element-start ((xtool xml-tool) name attrs)
;;    (format t "parse start ~a ~a ~a~%" xtool name (xml-tool.stack xtool))
  
  (unless (xml-tool.stack xtool)
    (let ((ptr (produce-xml-object (xml-tool.factory xtool) name)))

      (when (and attrs ptr)
	(init-attributes ptr attrs))
      
      ;; let's try to push instead..
      (push ptr (xml-tool.top-objects xtool))
      (push ptr (xml-tool.stack xtool))))


  (let ((ptr (element-start (car (xml-tool.stack xtool)) name attrs xtool)))
    (when ptr
      (push ptr (xml-tool.stack xtool)))))



(defmethod parse-element-end ((xtool xml-tool) name)
  
  (let ((thestack (xml-tool.stack xtool)))
    ;;    (assert (length thestack))
    (when (element-end (car thestack) name xtool)
      (pop (xml-tool.stack xtool)))))


(defmethod parse-element-content ((xtool xml-tool) content)
  (when (xml-tool.want-content xtool)
    (element-content (car (xml-tool.stack xtool)) content xtool)))



;; tune later
(defun parse-attribute (str)
  "Parses an attribute and returns a CONS with the info"
  
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (declare (type simple-string str))

  (let* ((first-space nil)
	 (first-val nil)
	 (xlen (- (length str) 1)))

    (declare (type fixnum xlen))
    
    (loop 
	for i of-type fixnum from 0 to xlen
	when (eq (schar str i) #\Space)
	do
	  (if first-space
	      (return-from parse-attribute (cons first-val (subseq str (1+ i))))
	    (progn
	      (setq first-val (subseq str 0 i))
	      (setq first-space t))))
    
    (cons first-val "")))

(defun create-attribute (xmlobj name type acc-ptr)
  "see (create-subelement ...)"
  ;;  (format t "creating attr ~a~%" name)

  (let ((ptr (slot-value xmlobj 'attr-info))
	(info (make-xml-attr-info name type acc-ptr)))

    (vector-push info ptr)))

(defun create-subelement (xmlobj name type acc-ptr)
  "adds a subelement to given xmlobj. Needs name and type and 
   an acc-ptr (just a symbol which can be used with (slot-value ..)"
  (let ((ptr (slot-value xmlobj 'sub-info))
	(info (make-xml-subelement-info name type acc-ptr)))

    (vector-push info ptr)))


(defun parse-xml-file (file-name)
  "takes a string as argument and calls xml2sexp. returns name of 
file with esis, or nil if something screwed up."
  (let ((res-file (make-temporary-filename))
	(program #+xml-sexp-reader *xml2sexp-prog*
		 #-xml-sexp-reader *xml2esis-prog*))
	    
    (when-verbose
     (albert-info "xml> executing command ~a ~a > ~a" program file-name res-file))
    
    ;; now let's run it
    
    (let ((retval (run-external-program program file-name :outfile res-file)))
      ;;(warn "Got retval ~s" retval)
      ;; nsgmls (if called) should have given a proper errmsg.. no?
      (if (= retval 0)
	  res-file
	  (progn
	    (wipe-file res-file)
	    nil))
      )))



(defun parse-xml (file-name xml-tool)
  "Takes two arguments, file-name which is a string and xml-tool which
should be an instance inited with a factory. Returns nil on failure."
  (let ((res-file (parse-xml-file file-name)))
    (when res-file
	
	 (return-from parse-xml 
		#+xml-sexp-reader	
		(read-sx-file res-file xml-tool)
		#-xml-sexp-reader	
		(read-esis-file res-file xml-tool))))
		
  nil)


(defun parse-typed-xml-file (fname factory type)
  "Returns the top-objects or NIL"

  ;; we need to get a decent filename
    
  (let* ((actual-fname (figure-out-fname fname))
	 (the-xmltool (make-xml-tool factory))
	 (parse-result (parse-xml actual-fname the-xmltool))
	 (retval nil))
	    
    (if parse-result
	(setq retval (xml-tool.top-objects the-xmltool))
	(warn "Something screwed up when parsing the ~a-file ~a, returning NIL"
	      type fname))

    retval))


#-sbcl
(declaim (ftype (function (simple-base-string) simple-base-string) xmlify-string))
;;(declaim (ftype (function (simple-base-string) simple-base-string) xmlify-string-1))


(defvar *tmpres-len* 8192 "keeps the length to avoid recalculation")
(defvar *tmpres* (make-string 8192) 
  "temporary array for the xmlify-string function")

(declaim (type fixnum *tmpres-len*))
(declaim (type simple-base-string *tmpres*))

(defun xmlify-string (str)
  "I don't know if it is any fast. I hope so. Uses *tmpres* to
save new string in."
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (declare (type simple-base-string str))
  
  (let ((cnt -1)
	(xlen (- (length str) 1)))
    
    (declare (type fixnum xlen cnt))
  
    ;; make sure the buffer is at least twice as long as string we
    ;; want to xmlify.  an educated guess
    (when (> (* xlen 2) *tmpres-len*)
      (setf *tmpres-len* (* 2 (if (> xlen *tmpres-len*) 
				  xlen 
				*tmpres-len*)))
      (setf *tmpres* (make-string *tmpres-len*)))
    
    (macrolet ((add-new-word (word)
		 ;; assume word is string
		 (let ((code nil))
		   (loop for x of-type base-char across word
		       do (push `(setf (schar *tmpres* (incf cnt)) ,x) code))
		   `(progn
		      ,@(nreverse code)))))
    
    
      (loop 
	  for i of-type fixnum from 0 to xlen 
	  for c of-type base-char = (schar str i)
	  do
	    (cond 
	     ((eq c #\&) (add-new-word "&amp;"))
	     ((eq c #\<) (add-new-word "&lt;"))
	     ((eq c #\>) (add-new-word "&gt;"))
	     ((eq c #\") (add-new-word "&quot;"))
	     (t (setf (schar *tmpres* (incf cnt)) c))))
      
      ;; faster way than subseq here?
      (subseq *tmpres* 0 (1+ cnt))
      )))


(defmethod print-object ((inst xml-tool) stream)
  (print-unreadable-object (inst stream :identity t)
			   (format stream "~:(~S~) [~S]" (class-name (class-of inst)) (xml-tool.factory inst)))
  inst)


(defmethod print-object ((inst xml-factory) stream)
  (print-unreadable-object (inst stream :identity t)
			   (format stream "~:(~S~) [~S]" (class-name (class-of inst)) (factory.name inst)))
  inst)


(defmethod verify-object ((obj xml-class) context)
  "calls DO-ITERATION with VERIFY-OBJECT-ACTUAL"
  (do-iteration obj #'verify-object-actual context))


(defmethod register-object ((obj xml-class) context)
  "calls DO-ITERATION with REGISTER-OBJECT-ACTUAL"
  (do-iteration obj #'register-object-actual context))


;; unused functions


#||
(defun xmlify-string-2 (str)
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (declare (type simple-base-string str))
 
  (apply #'concatenate 'string
         (loop for c of-type base-char across str
             collect (case c
                       (#\& "&amp;")
                       (#\< "&lt;")
                       (#\> "&gt;")
                       (t (string c))))))

(defun xmlify-string-3 (str)
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (declare (type simple-base-string str))
  
  (let ((res (make-array '(0) :fill-pointer t :adjustable t 
                         :element-type 'character)))
    
    (flet ((app-string (str)
             (loop for c of-type base-char across str do
                   (vector-push-extend c res))))
      (loop for c of-type base-char across str do
            (case c
              (#\& (app-string "&amp;"))
              (#\< (app-string "&lt;"))
              (#\> (app-string "&gt;"))
              (t (vector-push-extend c res)))))
    res))
                         


(defvar *entities* (make-array '(256) :initial-element nil)
  "A vector that maps character codes of XML entities (used as index) 
to the strings that name the entities. E.g, #\& -> &amp;")

(setf (aref *entities* (char-code #\&)) "&amp;")
(setf (aref *entities* (char-code #\<)) "&lt;")
(setf (aref *entities* (char-code #\>)) "&gt;")
(setf (aref *entities* (char-code #\>)) "&quot;")

(defun xmlify-string-4 (str)
  
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  (declare (type simple-base-string str))
  (let ((res (make-array 
              ;; initial guess for length of result. 
              (list (truncate (* (length str) 15) 10))
              :fill-pointer 0 :adjustable t 
              :element-type 'base-char)))
    (flet ((app-string (str)
             (loop for c of-type base-char across str do
                   (vector-push-extend c res))))
      
      (loop for c of-type base-char across str
          do
            (let ((s2 (aref *entities* (char-code c))))
              (if s2
                  (app-string s2)
                (vector-push-extend c res)))))
    
;;    (describe res)
    
    res))
||#


#||
(declaim (inline subelement-compare attr-compare))

(defun subelement-compare (name sub-obj)
  (declare (type simple-base-string name))
  (declare (type xml-subelement-info sub-obj))
  (if (string-equal name (the string (xml-subelement-info-name sub-obj)))
      sub-obj
      nil))
||#

#||
(defun attr-compare (name attr-obj)
  (declare (type simple-string name))
  (declare (type xml-attr-info attr-obj))
  (if (string-equal name (xml-attr-info-name attr-obj)) 
      attr-obj
      nil))
||#


#||
(declaim (ftype (function (fixnum) simple-string) get-indent))
(declaim (inline get-indent))
(defun get-indent (howmany)
  "Returns (* 2 howmany) spaces"  
  (declare (type fixnum howmany))
  (svref *indents* howmany))
||#


;; unused
#||
(defun get-hex-from-code (code next-code)
  "it currently returns a string with the iso-8859-1 char and not hex,
and it works when the original file was iso-8859-1"
  (declare (type fixnum code next-code))
  (let (first-val 
	second-val)
    (declare (ignore first-val))
    (declare (type fixnum first-val second-val))
    (setf first-val (ldb (byte 8 2) (- code #xC0)))

    (setf second-val (ldb (byte 8 0) (* code 64)))
    (setf second-val (logior second-val (- next-code #x80)))
    ;; (format t "Got ~a and ~a~%" first-val second-val)
    ;; latin:
    (format nil "~a" (code-char second-val))
    ;; hex:
;;;    (format nil "&#x~X~X;" 
;;;	    (if (= 0 first-val) "00" first-val) 
;;;	    (if (= 0 second-val) "00" second-val))
    ))
||#


#||
(defun subelement-match (name vec)
  (declare (type (simple-array XML-Subelement-Info)))
  (let ((len (- (length vec) 1))
	(val nil))
    (declare (type fixnum len))

    (loop for i of-type fixnum from 0 to len
	  do (setq val (aref vec i))
	  (if (string-equal name (subelement-info.name val))
	      (return-from subelement-match val)))
    nil))
||#

