;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/vars.lisp - the central variables of the spres package
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres-impl)

(defvar +id-word-delim+ "X")


;;; Variables meant for rule-code.. the ?NAME indicates that
;;; rules are allowed to change/rebind these.  Rules will not
;;; change/rebind *NAME* variables.

(defvar ?obj nil "The current object being treated.")
(defvar ?doc nil "The current document.")
;;(defvar ?object nil "Alias to ?obj.") 
;;(defvar ?document nil "Alias to ?doc.")

(defvar ?format nil "The format being used.")
(defvar ?context nil "The current context.")
(defvar ?parent nil "The parent object.")
(defvar ?language nil "The natural language we use.")
(defvar ?prog-lang nil "The programming language we use.")
(defvar ?outdir nil "The directory to do output to.")
(defvar ?repository nil "The object repository.")
(defvar ?class-hierarchy nil "The class-hierarchy.")
(defvar ?file-table nil "The table of which files should be made.")
(defvar ?rec-state nil "Which recursive state one is in.")
(defvar ?list-style :clever-sort "How to present content-lists.")

(defvar ?idx-register (make-hash-table :test #'equal))

(defvar *rule-trace* nil "Specifies whether we trace rules.")
(defvar *available-formats* (make-hash-table :test #'equal)
  "The format constructors.")

(defvar *scope-stack* '())
(defvar *spres-flags* '())

(defvar *clhs-table* (make-hash-table :test #'equal))
(defvar *clhs-root* (albert-setting '("hyperspec" "root")))

(defvar *enclosing-package* '() "Pointer to the enclosing SDOC-PACKAGE if it's there.")
(defvar *package-exports* nil
  "Simple table with all the exported symbols from *enclosing-package*")

;; move me
(defun is-prog-lang? (lang)
  ""
  (eq ?prog-lang lang))


#||
(defvar *class-hierarchy* nil 
  "dynamically bound variable with the class-hierarchy")

(defvar *content-repository* nil
  "dynamically bound variable with all tables with content")

(defvar *output-proglang-style* nil
  "set to a kwd-arg for language, e.g :lisp, :java, ...")

(defvar *current-output-level* 1
  "how far down are we when outputting stuff, 1,2,3, ...")

(defvar *current-output-dir* +default-output-dir+
  "should be bound for documents trying to write to hierarchies.")

(defvar *current-output-language* nil
  "should be bound to wanted language.")

(defvar *current-output-format* nil
  "should be bound to wanted format.")

||#
