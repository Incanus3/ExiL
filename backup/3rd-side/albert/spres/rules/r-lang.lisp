;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/r-lang.lisp - rule-code
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

;;; This file is scary.. Noone understands it

(in-package :spres-impl)

(defstruct rule-info
  key
  name
  demands
  desc
  variation)

(defstruct rule-def
  name
  args
  (print-execute :default)
  (on-fall-through :nothing))

(defstruct rule-arg
  keyword
  name
  rebind)

(defvar *rule-code* (make-hash-table :test #'equal) "The program code for the different rules.")
(defvar *rule-defs* (make-hash-table) "The definitions for the rules.")
(defvar *rules* (make-array 20 :fill-pointer 0) "An array of the RULE-INFOs.")
(defvar *generated-defuns* nil "A handy dynamic variable when generating DEFUNs.")

(defvar *verbose-rule-handling* nil "Should all rule-handling be verbose")


(defun rl-ensure-rule-form (key args variation)
  "ensures that the rule-form is registered."

  (let* ((arg-defs (loop for x in args
			 collecting (make-rule-arg :keyword (first x)
						   :name (second x)
						   :rebind (third x))))
	 (rule-def (make-rule-def :name key
				  :args arg-defs)))

    (when variation
      (when (find :never-print-execute variation)
	(setf (rule-def-print-execute rule-def) nil))
      (when (find :warn-on-fall-through variation)
	(setf (rule-def-on-fall-through rule-def) :warn))
      (when (find :error-on-fall-through variation)
	(setf (rule-def-on-fall-through rule-def) :error)))
    
    (setf (gethash key *rule-defs*) rule-def)

    
    (values)))

(defun rl-ensure-rule (information function)
  "ensures that the given rule is registered."

  (let ((key :not-set)
	(rule-name :not-set)
	(desc nil)
	(demands :not-set)
	(variation nil))
  
    ;; we must check the information, which should be a list
    (dolist (i information)
      ;; all info-objs should be of type cons
      (assert (consp i))

      (case (car i)
	(:key
	 
	 (setf key (cadr i))
	 (unless (or (symbolp key) (stringp key))
	   (error "The key to the rule must be a symbol or a string.")))
	
	(:name
	 (setf rule-name (cadr i))
	 (unless (symbolp rule-name)
	   (warn "The rule-name is not a symbol.")
	   ;; add check if rule-type exists
	   ))

	(:desc
	 (setf desc (cadr i))
	 (unless (stringp desc)
	   (error "The description given to DEF-RULE-INFO must be a string.")))

	(:req
	 (setf demands (cdr i))
	 ;;(warn "Demands ~a" demands)
	 )

	(:variation
;;	 (warn "Variation ~a" (cdr i))
	 (setf variation (cdr i)))
	))

    (when (eq key :not-set)
      (error "Every rule need a key."))
    (when (eq rule-name :not-set)
      (error "Every rule need a name."))
    (when (eq demands :not-set)
      (error "Every rule needs some demands to allow for gracious dispatch."))

  
  ;; first let us check that the rule is defined.
  (let ((rule-def (gethash rule-name *rule-defs*)))
    (unless rule-def
      (warn "When trying to register rule [id: ~a], a ~
rule-definition for ~a was not found." key rule-name)))
  
  (let ((pos (position key *rules* :key #'rule-info-key))
	(new-rule (make-rule-info :key key
				  :name rule-name
				  :demands demands
				  :desc desc
				  :variation variation)))
    (setf (gethash key *rule-code*) function)

;;    (warn "NEW rule: ~s" new-rule)
    
    (cond ((not pos)
	   (when *verbose-rule-handling*
	     (warn "Registering rule ~a" key))
	   (vector-push-extend new-rule *rules*))
	  (t
	   (when *verbose-rule-handling*
	     (warn "Updating rule ~a" key))
	   (setf (aref *rules* pos) new-rule)))

    (values))))

(defun rl-get-rule-def (name)
  (gethash name *rule-defs*))

(defmacro def-rule-info (information &body code)
  "Defines a presentation rule to be followed later."

  (assert (consp information))

  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (rl-ensure-rule ',information ',code)))

(defmacro def-rule-form (name args &optional variation)
  "Defines a rule-form, with the given name.  The list args
contains the arguments where each argument is a list:

 (REQ-KEYWORD LOC-VAR-NAME REBIND-VAR)

 REQ-KEYWORD is e.g ?class and refers to the keyword to use in
             a :req argument for dispatch
 LOC-VAR-NAME is the name of the local variable within the rule
 REBIND-VAR is the name of the var to rebind to point to the
      LOC-VAR-NAME.  It's mainly for convenience, and when NIL
      it is skipped

The list VARIATION contains keywords that might be interpreted
in special ways. 
 "
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (rl-ensure-rule-form ',name ',args ',variation)))

(defun rl-make-defun-name (rule-name key)
  (concat-pnames rule-name "-" key))

(defun rl-make-cond-clause (rule)
  "Returns an appropriate COND-clause for a rule."
  (let* ((demands (rule-info-demands rule))
	 (key (rule-info-key rule))
	 (rule-name (rule-info-name rule))
	 (the-funcall `(,(rl-make-defun-name rule-name key)
			,@(rl-get-arg-names rule))))
    (if demands
	`((and ,@demands) ,the-funcall)
	(list t the-funcall))))

(defun rl-get-arg-names (rule)
  "Return a list of variable-names"
  (let ((rule-def (rl-get-rule-def (rule-info-name rule))))
    (mapcar #'rule-arg-name (rule-def-args rule-def))))

(defun rl-generate-method (out-stream signature rules)
  "Generates a method and it's functions."
  
  (when *verbose-rule-handling* 
      (warn "Generating method ~a for ~a" signature (rule-info-key (car rules))))

  (let ((rule-def (rl-get-rule-def (rule-info-name (car rules)))))
    
    ;; we should do defuns first..
    (dolist (i rules)
;;      (warn "~s -> VAR: ~i " i (rule-info-variation i))
      (let* ((its-key (rule-info-key i))
	     (its-name (rule-info-name i))
	     (possible-notify-p (if (eq (rule-def-print-execute rule-def) nil)
				    nil
				    (if (find :never-print-execute (rule-info-variation i))
					nil
					t)))
	     (args (rl-get-arg-names i))
	     
	     
	     (defun-code `(defun ,(rl-make-defun-name its-name its-key)
			   ,args
			   ,(rule-info-desc i)
			   ,(when possible-notify-p (list 'rule-notify-start its-key))
			   (block ,its-name
			     ,@(gethash (rule-info-key i) *rule-code*)))))
	(pprint defun-code
		out-stream)
	(terpri out-stream)
	))
  
    (terpri out-stream)
    (terpri out-stream)

    (let ((sorted-rules rules)
	  (default-clause nil)
	  ;;(meth-content nil)
	  ;; only use base-obj
	  (on-fall-through (rule-def-on-fall-through rule-def))

	  )
    
      (when (> (length rules) 1);; opt?
	(setq sorted-rules (stable-sort rules #'>
					:key #'(lambda (x) (length (rule-info-demands x))))))

      ;; we can assume that the last one is the one which can be NIL
      (let ((poss-nil (last sorted-rules)))
	(when (and poss-nil (eq (rule-info-demands (car poss-nil)) nil))
	  (setf default-clause (car poss-nil))
	  (setf sorted-rules (remove (car poss-nil) sorted-rules))))

      (when *verbose-rule-handling*
	  (warn "Going along with [~s] vs [~s]" sorted-rules default-clause))
    
      (let ((meth-content nil))
	(cond ((and (not sorted-rules) default-clause)
	       ;; this can be optimised
	       (setq meth-content `(,(rl-make-defun-name (rule-info-name default-clause)
							 (rule-info-key default-clause))
				    ,@(rl-get-arg-names default-clause))))
	      (sorted-rules
	       ;; we have normal clauses
	       (let ((clauses (mapcar #'rl-make-cond-clause sorted-rules)))
		 
		 (cond (default-clause 
			   (setf clauses (nconc clauses (list (rl-make-cond-clause default-clause)))))
		       (t
			(ecase on-fall-through
			  (:nothing) ;; do nothing
			  (:warn
			   (setf clauses (nconc clauses (list `(t (warn "Fell through ~s and obj is ~s"
								   ',(first signature)
								   ?obj
								   ))))))
			  (:error
			   (setf clauses (nconc clauses (list `(t (error "Fell through ~s and obj is ~s"
								   ',(first signature)
								   ?obj
								   ))))))
			  )
			;;(warn "No default clause for ~s ~s" (first signature) clauses)
			))
		 
		 (setf meth-content `(cond ,@clauses))))
	      (t 
	       (error "weird situation ~a, ~a" sorted-rules default-clause)))
      
	     
	(let* ((args (let ((ptr (cdr signature)))
		       (mapcar #'(lambda (x) (prog1
						 (list (rule-arg-name x) (car ptr))
					       (setf ptr (cdr ptr))))
			       (rule-def-args rule-def))))
	       (meth-code `(defmethod ,(first signature) ,args
			    (let (,@(let ((ptr (cdr signature)))
					 (filter #'(lambda (x)
						     (prog1
							 (let ((rebind-name (rule-arg-rebind x)))
							   (when rebind-name
							     (list rebind-name
								   (rule-arg-name x))))
						       (setf ptr (cdr ptr))))
						 (rule-def-args rule-def))))
			      ,meth-content))
			
		 ))

	
	  (pprint meth-code out-stream)))

      (terpri out-stream)
      )))
  
(defun generate-rule-code (filename)
  "generates code for the rules to the given file"

  (let ((signatures (make-hash-table :test #'equal))
	(*generated-defuns* nil)
	(*print-right-margin* 80)
	(*print-circle* nil)
	(*print-case* :downcase))
    
    (loop for x across *rules*
	  for sign = (rl-get-signature x)
	  do
	  (let ((cleaned-rule (rl-clean-basic-req x)))
	    ;;(warn "Gone from ~s to ~s" x cleaned-rule)
	    (push cleaned-rule (gethash sign signatures))))

    
    (with-open-file (out-file (merge-pathnames filename)
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
      (format out-file ";;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SPRES-IMPL -*-~%")
      (format out-file ";;; Please do not edit this file.  It is auto-generated.~2%")

      (format out-file "(in-package :spres-impl)~2%")
      (format out-file "#+cmu (declaim (optimize (ext:inhibit-warnings 3)))~2%")
      
      (maphash #'(lambda (sign rules)
		   (rl-generate-method out-file sign rules))
	       signatures)
    
      )))


(defun rl-clean-basic-req (rule)
  "no side-effects.. returns a new rule without basic keywords in
the demand list."
  (let* ((rule-name (rule-info-name rule))
	 (new-rule (make-rule-info :key (rule-info-key rule)
				   :name rule-name
				   :demands nil
				   :desc (rule-info-desc rule)
				   :variation  (rule-info-variation rule)))
	 
	 (rule-def (rl-get-rule-def rule-name))
	 (keywords (loop for x in (rule-def-args rule-def)
			 collecting (rule-arg-keyword x)))
	 (clean-list (loop for x in (rule-info-demands rule)
			   collecting (if (and (consp x) (find (car x) keywords))
					  nil
					  x))))
    (setf clean-list (remove nil clean-list))
;;    (warn "{~a} \\ {~a} -> {~a}" (rule-info-demands rule) keywords clean-list)

    (block verification
      (let ((list-to-do (if (and clean-list (eq (car clean-list) '?req))
			    (cdr clean-list)
			    clean-list)))
	(dolist (i list-to-do)
;;	  (warn "Checking ~a" i)
	  (let* ((sym (car i))
		 (name (symbol-name sym)))
	    (when (eql (char name 0) #\?)
	      (warn "Remaining keyword ~s in rule ~s"
		    sym (rule-info-key rule)))))))
    
    (when (> (length clean-list) 0)
      (setf (rule-info-demands new-rule) clean-list))

;;    (warn "{~s} -> {~s}" rule new-rule)
    
    new-rule))

(defun rl-get-poss-quoted-val (dem)
  "Returns the value of a symbol and bypasses any quote."
  (if (and (consp dem) (eq (car dem) 'quote))
      (cadr dem)
      dem))

(defun rl-get-signature (rule)
  "Returns the appropriate signature for the given rule."

  (let ((demands (rule-info-demands rule))
	(sign-acc nil))

    (when (rl-verify-demands demands)
      (let* ((dem-list demands)
	     (rule-def (rl-get-rule-def (rule-info-name rule))))

	(dolist (i (rule-def-args rule-def))
	  (let ((appropriate-demand (find (rule-arg-keyword i) dem-list :key #'car)))
	    (if appropriate-demand
		(push (rl-get-poss-quoted-val (cadr appropriate-demand)) sign-acc)
		(push t sign-acc))))))
    

    (append (list (rule-info-name rule)) (nreverse sign-acc))
    ))

(defun rl-verify-demands (demands)
  "Verifies that the demand-list is peachy.  Not complete."
  (let ((retval t))
    (unless (listp demands)
      (warn "Weird start of demand-list for rule: ~a" demands)
      (setq retval nil))
    
    retval))

(defun rl-clean-tables ()
  "Cleans all rule-tables."
  (clrhash *rule-code*)
  (clrhash *rule-defs*)
  (setf (fill-pointer *rules*) 0))
	   
