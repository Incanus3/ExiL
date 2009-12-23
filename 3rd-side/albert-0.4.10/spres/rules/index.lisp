;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/rules/sort.lisp - sorting of content for presentation
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres-impl)

(def-rule-info
    ((:key :pck-idx/docbook)
     (:name present-object)
     (:desc "index a package")
     (:req (?class sdoc-package)
	   (?document-type docbook-document)
	   (?context (eql :index))))

    (let ((content (slot-value obj 'content)))
      (put doc "<refsect1><title>Index for package " (get-object-name obj) "</title>" (eol))

      (let* ((?parent obj))
	(db-insert-index doc content (if (is-prog-lang? :lisp)
					 2
					 3)
			 :only-link-existing nil))
      
      (put doc "</refsect1>" (eol))
      ))





