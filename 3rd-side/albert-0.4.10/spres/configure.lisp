;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SPRES-IMPL -*-

#|

DESC: spres/configure.lisp - spres configurations
Copyright (c) 1998-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :spres-impl)

;; titles/words used in docs can be configured here mostly
;; english is base language, so most of this is redundant, but
;; it shows which words are accessed by Albert in case you might
;; want to configure or add another language

(define-language English :en
  ("and" "and")
  ("function" "function")
  ("variable" "variable")
  ("constructor" "constructor")
  ("Calls" "Calls")
  ("Called By" "Called By")
  ("Variables" "Variables")
  ("Variables and Constants" "Variables and Constants")
  ("Constants" "Constants")
  ("Methods" "Methods")
  ("Related Methods" "Related Methods")
  ("Uses" "Uses")
  ("Exports" "Exports")
  ("slot" "slot")
  ("constant" "constant")
  ("scope" "scope")
  ("location" "location")
  ("inherits" "inherits")
  ("class-variable" "class-variable")
  ("# variables" "# variables")
  ("# methods" "# methods")
  ("Slots" "Slots")
  ("Struct details" "Struct details")
  ("Class details" "Class details")
  ("Package Content" "Package Content")
  ("Indexes" "Indexes")
  ("Global Index" "Global Index")
  ("Class Hierarchy" "Class Hierarchy")
  ("Description" "Description")
  ("is of type" "is of type"))

(define-language Norsk :no
  ("Classname" "Klassenavn")
  ("Modulename" "Modulnavn")
  ("Index"    "Oversikt")
  ("index"    "oversikt")
  ("hierarchy" "hierarki")
  ("Hierarchy" "hierarki")
  ("and"    "og")
  ("Summary"    "Sammendrag")
  ("Content"    "Innhold")
  ("Module"    "Modul")
  ("Method"    "Metode")
  ("method"    "metode")
  ("Methods"   "Metoder")
  ("class-method"    "klassemetode")
  ("function"    "funksjon")
  ("destructor"    "destruktør")
  ("constructor"    "konstruktør")
  ("Variable"    "Variabel")
  ("variable"    "variabel")
  ("Variables"   "Variabler")
  ("class-variable"    "klassevariabel")
  ("constant"    "konstant")
  ("Class"    "Klasse")
  ("class"    "klasse")
  ("Classtype"    "Klassetype")
  ("SDS-Generated documentation"    "SDS-generert dokumentasjon")
  ("Toplevel"    "Toppnivå")
  ("starts at line"    "starter på linje")
  ("at scope"    "i 'scope'")
  ("Location"    "Plassering")
  ("location"    "plassering")
  ("Access"    "Tilgang")
  ("is of type"    "er av type")
  ("is type-alias for"    "er type-alias for")
  ("Inherits"    "Arver")
  ("Description"    "Beskrivelse")
  ("Subclasses"    "Subklasser")
  ("Sketch"    "Skisse")
  ("Time"    "Tidsbruk")
  ("Version"    "Versjon")
  ("Data Structure"    "Datastruktur")  
  ("Calls" "Kaller")
  ("Called by" "Kalt av")
  ("Category" "Kategori")
  ("Throws" "Kaster")
  ("is at line" "er på linje")
  ("is on lines" "er på linjene")
  ("has values" "har verdier")
  ("See Also" "Se også")
  ("Quick-index" "Kjappindeks")
  ("# variables" "# variabler")
  ("# methods" "# metoder")
  ("interfaces" "grensesnitt")
  ("inherits" "arver")
  ("Class details" "Klassedetaljer")
  )


(defun %add-doc-kwd-mappings (table)
  "adds doc kwd mappings to the given table."
  
  (setf (gethash "desc" table) "Description"
	(gethash "time" table) "Time"
	(gethash "memo" table) "Memo"
	(gethash "sketch" table) "Sketch"
	(gethash "equiv" table) "Equivalent"
	(gethash "remark" table) "Remark"
	(gethash "returns" table) "Returns"
	(gethash "param" table) "Parameters"
	(gethash "dstruct" table) "Data Structure"
	(gethash "version" table) "Version"
	(gethash "see" table) "See Also"
	(gethash "calls" table) "Calls"
	(gethash "calledby" table) "Called By"
	(gethash "throws" table) "Throws"
	))

(%add-doc-kwd-mappings *documentation-kwd*)

(establish-doc-handler& "see" #'handle-see-doc)
(establish-doc-handler& "param" #'handle-param-doc)
(establish-info-handler& "calls" #'handle-calls-info)
(establish-info-handler& "calledby" #'handle-calls-info)
(establish-info-handler& "throws" #'handle-calls-info)

;;(add-doc-handler "mod" #'handle-mod-doc)

