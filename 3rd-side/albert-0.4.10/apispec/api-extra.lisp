;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: APISPEC -*-

#|

DESC: apispec/api-extra.lisp - extra functions for the APISpec API
Copyright (c) 1999,2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :sds-api-apispec)

(defun parse-apispec-file (fname)
  "Returns the top-objects or NIL"

  (parse-typed-xml-file fname (make-apispec-factory) "APISpec"))

