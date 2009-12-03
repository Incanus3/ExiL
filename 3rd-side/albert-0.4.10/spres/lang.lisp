;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/lang.lisp - languages is my guess
Copyright (c) 1998,1999 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres-impl)


(defvar *installed-languages* (make-hash-table :test #'equal) 
  "hashtable with language codes")

;; abstract
(defclass SPRES-Language () () )


;;; creates two functions:
;;; fill-in-lang-words-for-NAME
;;; remove-lang-words-for-NAME
;;;   both take one argument, a hashtable to fill in lang-words
;;; also creates class Language-NAME which inherits SPRES-Language
;;; Finally adds an entry on CODE in *installed-languages* with 
;;;   above mentioned functions in a simple cons

(defmacro define-language (name code &rest word-lists)
  (let ((add-clauses nil)
	(rem-clauses nil)
	(ignore-input nil)
	(class-name (concat-pnames "Language-" `,name))
	(add-name (concat-pnames "fill-in-lang-words-for-" `,name))
    	(rem-name (concat-pnames "remove-lang-words-for-" `,name))
	(hash-sym (gensym "hashtable")))

    (cond ((eq word-lists nil)
	   (setq ignore-input `(declare (ignore ,hash-sym))))
	  (t
	   (dolist (x word-lists)
	     (push `(setf (gethash ,(car x) ,hash-sym) ,(cadr x)) add-clauses)
	     (push `(remhash ,(car x) ,hash-sym) rem-clauses))))

    `(progn
      (defclass ,class-name (spres-language) () )
      (defun ,add-name (,hash-sym)
	,ignore-input
	,@add-clauses
	(values))
      (defun ,rem-name (,hash-sym)
	,ignore-input
	,@rem-clauses
	(values))
      (setf (gethash (string-downcase (string ,code))
	     *installed-languages*)
       (cons #',add-name #',rem-name))
      )))

;;; language functions (scoped variable the-lang)
;;; will be automatically generated later.. 
(defvar *current-language* nil) ;; this used to be a closed variable

(defun check-for-language (name)
  "Returns entry in table of installed languages."
  (gethash (string-downcase (string name)) *installed-languages*))

(defun remove-language (lang-table)
  ;; not sure whether clrhash is better suited here..
  ;;   use inverse of set until later..
  (when *current-language*
    (let ((old-lang (check-for-language *current-language*)))
      (if old-lang
	  (funcall (cdr old-lang) lang-table);; removing old entries
	  (warn "Albert/Language: Unknown language ~a" *current-language*)))))
  
(defun set-language (lang lang-table)
  (when (not (equal *current-language* lang))
    (when *current-language*
      (remove-language lang-table))
    (setq *current-language* lang)
    (let ((new-lang (check-for-language *current-language*)))
      (if new-lang
	  (funcall (car new-lang) lang-table);; add new entries
	  (warn "Albert/Language: Unknown language ~a" *current-language*)))))

(defun make-language-table()
  (make-hash-table :test #'equal))

(defun get-language (lang)
  ;;(warn "someone gets language ~a" lang)
  (let ((the-table (make-language-table)))
    (set-language lang the-table)
    the-table))

;; could be inlined I guess
(declaim (inline get-word))
(defun get-word (word document)
  "makes a look up for a word in appropriate hash table"
  (let ((res (gethash word (document.lang-words document))))
    (cond (res
	   res)
	  (t
	   (when-verbose
	       (albert-warn "No translation for 'word' ~s in language ~s"
			    word *current-language*)) ;; not quite right, but a guess
	   word))))


(defvar *documentation-kwd* (make-hash-table :test #'equal)
  "a table with translations from documentation keyword to the
full word")

(defun get-doc-word (word document)
  "Returns a translated word for a documentation kwd"
  (let ((gotten-word (gethash word *documentation-kwd*)))
    (if gotten-word
	(get-word gotten-word document)
	word)))

;; this needed? yes
;;(memoize 'get-language :test #'equal)
;;(memoize 'get-doc-word :test #'equal)

