;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: APISPEC-XML -*-

#|

DESC: apispec/xml-esis.lisp - takes care of access to xml-parser through shell-call
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :apispec-xml)

;; can maybe be tuned somewhat
(defun read-esis-file (filename xml-tool)
  "filename is assumed to be a pathname and xml-tool a real xml-tool inited with 
a factory"
  
  (declare (optimize (safety 0) (speed 3) (debug 0)))
  
  ;;  (format t "Read esis file ~a with ~a~%" filename xml-tool)
  
  (let* ((attr-stack nil)
	 (rest-of-line "")
	 (last-pos 0)
	 (cont-collector nil)
	 ;; this may slow things down and be outright annoying
	 (full-file (eat-file filename))
	 (len (- (length full-file) 1)))
    
    (declare (type fixnum len last-pos))
    (declare (type simple-base-string full-file rest-of-line))


    (loop 
	for i of-type fixnum from 1 to len 
	for cur-char of-type character = (schar full-file i)
	when (eq cur-char #\Newline)
	do
	  ;;	  (when (= i len) (return))
	  (setq rest-of-line (subseq full-file (1+ last-pos) i))
	  
	  (case (schar full-file last-pos)
	    (#\(   (parse-element-start xml-tool rest-of-line attr-stack)
		   (setq attr-stack '())
		   )
	    (#\) (when cont-collector
		   (parse-element-content xml-tool cont-collector)
		   (setq cont-collector nil))
	      (parse-element-end xml-tool rest-of-line))
	    (#\A (push (parse-attribute rest-of-line) attr-stack))
	    (#\- (if cont-collector 
		     (setq cont-collector (strcat cont-collector rest-of-line))
		   (setq cont-collector rest-of-line)))
	    (#\? "do nothing")
	    (#\C "do nothing")
	    (otherwise (error "Unknown lineformat: ~A" rest-of-line))
	    )
	  (setq last-pos (1+ i)))
    
    
    ;; get rid of temp-file
    (wipe-file filename)
    
    ))

