;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LISP2CSF -*-

#|

DESC: lisp2csf/cleaning.lisp - preprocesses lisp-code for safer reading
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

-------

|#


(in-package :lisp2csf)

(defvar +end-of-line+ '(#\Newline #\Return))
(defvar +alphabetic+ '(#\& #\% #\! #\: #\- #\_ #\+ #\* #\> #\< #\= #\. #\/ #\@ #\$ #\^))
(defvar +ignore-chars+ (list #\( #\) #\Space #\Tab #\' #\` #\~ #\Newline #\Return #\, #\@ #\[ #\]
			     (code-char 12) #\|))
(defvar +numeric+ '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\0))

(defun figure-out-lisp-packages (fname)
  "Returns a list with strings which name external symbols
referenced in the file."
  
  (let ((full-file (eat-file fname)))
    (handler-case
	(find-lisp-packages-from-string full-file :fname fname)
      (cl:end-of-file ()
	nil))))
  

(defun find-lisp-packages-from-string (the-string &key (fname "<string>"))
  "Tries to find lisp-packages from the given string."

  (let ((len (1- (length the-string)))
	(tokens nil))

    (labels ((get-char-num (num)
	       (if (< num len)
		   (schar the-string num)
		   (signal 'cl:end-of-file)))
	     
	     (skip-line-from (num)
	       (loop for x from 0
		     for j from num
		     do
		     (when (or (> j len)
			       (find (get-char-num j) +end-of-line+))
		       (return-from skip-line-from x))))
	     
	     (skip-string-from (num)
	       (let ((last-char #\Space))
		 (loop for x from 1
		       for j from (1+ num)
		       do
		       (when (> j len)
			 (warn "Too long string..."))
		       (let ((the-char (get-char-num j)))
			 (when (and (eq the-char #\")
				    (not (eq last-char #\\)))
			   (return-from skip-string-from x))
			 (setq last-char the-char)
			 ))))
	     
	     (skip-token-from (num)
	       (length (read-token-from num)))
	     
	     (read-and-analyse-token-from (num)
;;	       (warn "reading new token from ~a" num)
	       (let ((token (read-token-from num)))
		 ;;(warn "pushing ~a atop ~a" token tokens) 
		 (push token tokens)
		 (length token)))

	     (read-token-from (num)
	       (loop for x from 0
		     for j from num
		     do
		     (when (> j len)
		       (let ((retval (subseq the-string num j)))
			 ;;(warn "Never-ending token at [~a]...-> ~a" j retval)
			 (return-from read-token-from retval)))
		     (let ((the-char (get-char-num j)))
		       (unless (or (alphanumericp the-char)
				   (find the-char +alphabetic+))
			 ;;(warn "Getting subseq from [~a,~a] to [~a,~a]" num (get-char-num num) j the-char)
			 (return-from read-token-from (subseq the-string num j))))) )

	     (skip-comment-from (num)
	       (loop for x from 0
		     for j from (1+ num)
		     do
		     (when (> j len)
			 (warn "Never-ending comment..."))
		     (let ((cur-char (get-char-num j)))
		       (when (and (eq cur-char #\#)
				  (eq (get-char-num (1- j)) #\|))
;;			 (warn "Skipping comment [~a]" (subseq the-string num (+ num x 2))) 
			 (return-from skip-comment-from x)))))
			 
	     
	     (read-and-analyse-hash-from (num)
	       (let ((next-char (get-char-num (1+ num))))
		 ;;(warn "next char is ~s" next-char) 
		 (cond
		   ((or (eql next-char #\x)
			(eql next-char #\X)
			(eql next-char #\b)
			(eql next-char #\B)
			(eql next-char #\c)
			(eql next-char #\C)
			)
		    (+ 2 (skip-token-from (+ 2 num))))

		   ((or (eql next-char #\')
			(eql next-char #\.)
			(eql next-char #\())
		    1)
		    
		   ((or (eql next-char #\\)
			(eql next-char #\+)
			(eql next-char #\-)
			(eql next-char #\o)
			(eql next-char #\O)
			(eql next-char #\p)
			(eql next-char #\[)
			(eql next-char #\])
			(eql next-char #\x)) ;; hack
		    (+ 2 (skip-token-from (+ 2 num))))
		   
		   ((eql next-char #\|)
		    ;;(warn "next-char after comment is ~a" (schar the-string (+ 2 (skip-comment-from (+ 2 num)))))
		    (+ 3 (skip-comment-from (+ 2 num))))
		   
		   ((or (find next-char +alphabetic+)
			(find next-char +ignore-chars+)
			(find next-char +numeric+))
		    2)
		   (t
		    (warn "Unknown hash-char ~a [~a] [file: ~a ]" next-char (char-code next-char) fname)
		    2))))
	     )


      
    (loop for i of-type fixnum from 0 to len
	  for cur-char = (get-char-num i)

	  do
;;	  (warn "checking ~s[~s][~s]" cur-char i len)
	  
	  (cond
	    ((eq cur-char #\;) (incf i (skip-line-from i)))
	    ((eq cur-char #\") (incf i (skip-string-from i)))
	    ((eq cur-char #\:) (incf i (skip-token-from i)))
	    ((find cur-char +ignore-chars+)
	     t)
	    ((or (alphanumericp cur-char)
		 (find cur-char +alphabetic+)
		 (eq cur-char #\?))
	     (incf i (read-and-analyse-token-from i)))
	    
	    ((eq cur-char #\#)
	     (let ((e i))
	       (declare (ignore e))
	       (incf i (read-and-analyse-hash-from i))
	       ;;(warn "Skipped {~a} {~a -> ~a [~a]}" (subseq the-string e i) e i (- i e))
	       ))
	    (t
	     (warn "Unknown char ~a [~a, pos: ~a, file: ~a ] {~a}"
		   cur-char (char-code cur-char)
		   i fname (subseq the-string (- i 5) (+ i 5)))

	     )))
	    
    

    )

    (let ((new-tokens (remove-duplicates tokens :test #'equal))
	  (real-tokens nil))
;;      (warn "Attacking tokens: ~a" new-tokens)
      (dolist (x new-tokens)
	(when (find #\: x)
	  (push x real-tokens)))
;;      (warn "Returning ~s" real-tokens)
      real-tokens)
    
    ))
