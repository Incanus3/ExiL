;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-

#|

DESC: tools/bug-fix.lisp - bug-fixing that may be relevant
Copyright (c) 2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :cl-user)

(defun bug-fixing ()
  
  #+(or clisp gcl) (declaim (declaration values))
    
  ;; fix it to only be 5.x
  #+(and allegro (or allegro-v5.0 allegro-v5.0.1))
  (progn
    ;; Duane Rettig <duane@franz.com>:
    ;; TIME reports 32 other bytes too many CL unless tuned with
    (setq excl::time-other-base 32)
    ;; From Erik Naggum <erik@naggum.no> [22 Feb 1999 10:27:45 +0000]
    ;; fixes the (read-from-string "[#\\x]") problem
;;    #-(and allegro (version>= 6))
    (loop with readtables = (excl::get-objects 11)
          for i from 1 to (aref readtables 0)
          for readtable = (aref readtables i) do
          (when (excl::readtable-dispatch-tables readtable)
            ;; reader for character names immune cltl1
            (set-dispatch-macro-character
             #\# #\\
             (excl::named-function
              excl::sharp-backslash
              (lambda (stream backslash font)
                (declare (ignore font))
                (unread-char backslash stream)
                (let* ((charstring (excl::read-extended-token stream)))
                  (unless *read-suppress*
                    (or (character charstring)
                        (name-char charstring)
                        (excl::internal-reader-error
                         stream "Meaningless character name ~A"
                         (string-upcase charstring)))))))
             readtable)))
    
    )
  #+gcl (defmacro lambda (bvl &body forms) `#'(lambda ,bvl ,@forms))
  #+allegro-v4.3                ; From Erik Naggum <erik@naggum.no>
  (unless (member :key (excl:arglist #'reduce) :test #'string=)
    (setq excl:*compile-advice* t)
    (excl:defadvice reduce (support-key :before)
      (let ((key (getf (cddr excl:arglist) :key)))
        (when key
          (remf (cddr excl:arglist) :key)
          (setf (second excl:arglist)
                (map 'vector key (second excl:arglist)))))))
  
  #-(or clisp allegro)
  (define-setf-expander values (&rest places &environment env)
    (loop :for pl :in places :with te :and va :and ne :and se :and ge :do
          (multiple-value-setq (te va ne se ge) (get-setf-expansion pl env))
          :append te :into te1 :append va :into va1 :append ne :into ne1
          :collect se :into se1 :collect ge :into ge1
          :finally (return (values te1 va1 ne1 (cons 'values se1)
                                   (cons 'values ge1)))))
  (values))

(defun optimisation-fixing ()
  "..."

    #+allegro
  (progn
;;    (setq excl:*record-source-file-info* nil
;;	  excl:*load-source-file-info* nil)
    (setq excl:*record-source-file-info* t
	  excl:*load-source-file-info* t
	  excl:*load-local-names-info* t
	  excl:*load-xref-info* t) 
    (setq compiler:save-local-names-switch t)
;;    (setf (sys:gsgc-parameter :free-bytes-new-other) 1048576
;;	  (sys:gsgc-parameter :free-bytes-new-pages) 1048576
;;	  (sys:gsgc-switch :gc-old-before-expand) t)
    )
    #+clisp
  (progn
    (format t "Removing some clisp-warnings.. we hope~%")
;;    (push (pathname "@lisppath@/") *load-paths*)	
    (setq 
     clos::*gf-warn-on-removing-all-methods* nil
     clos::*warn-if-gf-already-called* nil
     clos::*gf-warn-on-replacing-method* nil
     system::*SOURCE-FILE-TYPES* '(".lisp" ".lsp")))

  (values))

