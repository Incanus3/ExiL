;;; This is a pretty complex example of a docme-script for albert
;;; and it documents albert itself, so this is the dogfood script
;;; most of the tricky stuff is not needed in other apps, but some
;;; parts may be of interest, e.g check-body-expression and
;;; how I delegate and handle work in a few ANALYSE-OBJECT methods
;;; these are also allowed to mimic macros and generate new code that
;;; needs documenting. 

(in-package :cl-user)

(setf cl:*load-verbose* nil
      cl:*load-print* nil)


(pushnew :sds-devel *features*)
(load "albert.asd" :verbose nil)
(asdf:oos 'asdf:load-op 'albert)

;; this file is mostly for me and this is where I keep it
(setf apispec:*xml2sexp-prog* "/home/stig/Projects/albert/expat/alb_xml2sexp")

(setf (albert:albert-setting '("albert" "verbose")) nil
      ;; I use these for debugging
      (albert:albert-setting '("albert" "use-temporary-files")) t 
      ;; will not interfere with tab-completion
      (albert:albert-setting '("albert" "lisp2csf" "outfile")) "sylte.xml"
      ;; for testing
      ;;(albert:albert-setting '("albert" "presentation" "only-exported")) t)
      ;; I'm in alfabeta mode
      (albert:albert-setting '("albert" "presentation" "class" "related-methods")) t
      )

;; don't want to know about these
(dolist (i '(+ - * / = < <= > >= /=
	     dotimes cons list push pushnew string dolist assoc
	     null consp integerp consp symbolp characterp stringp
	     listp numberp typep minusp plusp check-type assert
	     decf incf 1- 1+ mod floor truncate
	     get getf nconc append
	     aref svref nth elt length list-length
	     first second third fourth fifth
	     caar cadr cddr 
	     #+cmu COMMON-LISP::BACKQ-APPEND
	     #+cmu COMMON-LISP::BACKQ-CONS
	     #+cmu COMMON-LISP::BACKQ-LIST
	     #+cmu COMMON-LISP::BACKQ-LIST*
	     ))
	     
  (pushnew i lisp2csf:*ignorable-calls*))

;; this is just a basic method, we forward the analysis
(defmethod lisp2csf:analyse-object ((objtype (eql 'defmethod-with-warn)) expression)
  (lisp2csf:analyse-object 'cl:defmethod (cons 'cl:defmethod (cdr expression))))

;; cheating hack
(defvar *current-sds-module* nil)

(defmethod lisp2csf:analyse-object ((objtype (eql 'define-sds-module)) expression)
  (setf *current-sds-module* (second expression))
  (lisp2csf:analyse-object 'cl:defconstant (list 'defconstant '+sds-module-name+ (second expression)))
  (lisp2csf:analyse-object 'cl:defvar (list 'defvar '*constructors* '(make-hash-table :test #'equal)))
  (lisp2csf:analyse-object 'cl:defun (list 'defun 'get-constructor '(name) '(gethash "name" *constructors*))))

(defmethod lisp2csf:analyse-object ((objtype (eql 'create-obj-constructors)) expression)
  (let ((exports '()))
    (loop for i in (cdr expression)
	  do
	  (progn
	    (assert (consp i))
	    (let* ((class-name (apispec-base:concat-pnames *current-sds-module* "-" (car i)))
		   (name (apispec-base:concat-pnames "MAKE-" class-name)))
	      (push name exports)
	      (lisp2csf:analyse-object 'cl:defun (list 'defun name '(&key (parent nil))
						       '(make-instance 'class-name :parent parent)))
	      )))
    (lisp2csf:analyse-object 'cl:export (list 'export exports))))


;; too damn tricky, and it's the essential part of albert.. sad
(defmethod lisp2csf:analyse-object ((objtype (eql 'def-sds-class)) expression)
  (let* ((name (second expression))
	 (exports '())
	 (cl-name (apispec-base:concat-pnames *current-sds-module* "-" name))
	 (slots (third expression))
	 (the-slots (mapcar #'(lambda (x)
				(push (apispec-base:concat-pnames cl-name "." x) exports)
				(list x :accessor (apispec-base:concat-pnames cl-name "." x)
				      :initarg (apispec-base:concat-pnames ":" x)
				      :initform nil
				      :type 'list)) slots))
	 (expr `(defclass ,cl-name (xml-class)
		,the-slots)))
    (push cl-name exports)
    ;;(warn "Found class ~s" expr)
    (lisp2csf:analyse-object 'cl:export (list 'export exports))
    (lisp2csf:analyse-object 'cl:defclass expr)))

;; tricky bugger too
(defmethod lisp2csf:analyse-object ((objtype (eql 'def-sds-const)) expression)
  t)

;; this one is more of a bastard, and has the worst code ever
(defmethod lisp2csf:analyse-object ((objtype (eql 'def-or-method)) expression)
  ;;(warn "OR for ~s with ~s" (second expression) (third expression))
  ;; we must gather a list of combos and analyse them all, we assume only one arg is OR'ed
  (let* ((maxlen 1)
	 (arglen (length (third expression)))
	 (args '()))
    (dolist (arg (third expression))
      ;;(warn "Checking ~s" arg)
      (when (and (consp arg)
		 (consp (second arg)))
	
	(assert (eq (car (second arg)) 'OR))
	(when (> (length (cdr (second arg))) maxlen)
	  (setf maxlen (length (cdr (second arg)))))))
    
    ;;(warn "Combinations ~s" maxlen)

    (setf args (make-list maxlen))
    (dotimes (i maxlen)
      (setf (nth i args) (make-list arglen)))

    ;;(warn "Loop -> ~s" args)
    
    (loop for argnum from 0
	  for arg in (third expression)
	  do
	  (progn
	    (cond ((and (consp arg)
			(consp (second arg)))
		   (assert (eq (car (second arg)) 'OR))
		   ;;(warn "Sec ~s" (cdr (second arg)))
		   (loop for i from 0
			 for a in (cdr (second arg))
			 do
			 (setf (nth argnum (nth i args)) (list (car arg) a))))
		  (t
		   (dotimes (i maxlen)
		     (setf (nth argnum (nth i args)) arg))
		   ))
	    ))

    ;;(warn "OR for ~s with ~s -> ~s" (second expression) (third expression) args)
    (dolist (arg args)
      (let ((expr (cons 'cl:defmethod
			(cons (second expression)
			      (cons arg (cdddr expression))))))
	;;(warn "We went from ~s to ~s" expression expr)
	(lisp2csf:analyse-object 'cl:defmethod expr)))
    t))


;;; Langband-specific handling of some macros
(defmethod lisp2csf:check-body-expression (expr-type expression)
  (cond ((equal (symbol-name expr-type) "WHEN-BIND")
	 (let ((var-calc (second (second expression))))
	   (lisp2csf:analyse-body-expression var-calc))
	 (map nil #'lisp2csf:analyse-body-expression (cddr expression))
	 t)
	((equal (symbol-name expr-type) "UNLESS-BIND")
	 (let ((var-calc (second (second expression))))
	   (lisp2csf:analyse-body-expression var-calc))
	 (map nil #'lisp2csf:analyse-body-expression (cddr expression))
	 t)
	((equal (symbol-name expr-type) "UNLESS-QUIET")
	 (map nil #'lisp2csf:analyse-body-expression (cdr expression))
	 t)
	((equal (symbol-name expr-type) "ALBERT-WARN")
	 (map nil #'lisp2csf:analyse-body-expression (cddr expression))
	 t)
	((equal (symbol-name expr-type) "ALBERT-INFO")
	 (map nil #'lisp2csf:analyse-body-expression (cddr expression))
	 t)

	(t nil)))


(albert:document-systems :albert)

;;(warn "Ran through tools/docme.lisp and got to the end.")

#+cmu
(when ext:*batch-mode*
  (quit))

