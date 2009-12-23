;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: sds-global -*-

#||

DESC: settings.lisp - various global settings
Copyright (c) 1998,1999,2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

||#

(in-package :apispec-base)

(defvar *xml2esis-prog* "/usr/bin/alb_xml2esis")
(defvar *xml2sexp-prog* "/usr/bin/alb_xml2sexp")


(in-package :sds-global)

(def-albert-setting '("albert" "use-temporary-files") nil
  "By default, use temporary files for intermediate results and the xml parser.  If this
is NIL, it will not use intermediate files and will not need the xml-parser.")

(def-albert-setting '("albert" "lisp2csf" "outfile") "lispy-csf.xml"
  "The default name of the lisp2csf outfile.")
  
(def-albert-setting '("albert" "csf2csf" "outfile") "linked.xml"
  "The default name of the csf2csf outfile.")
  
(def-albert-setting '("albert" "csf2sdoc" "outfile") "converted.xml"
  "The default name of the csf2sdoc outfile.")

(def-albert-setting '("albert" "presentation" "language") "english"
  "The default natural language for spres/sdoc.")
  
(def-albert-setting '("hyperspec" "root")
    "http://www.lispworks.com/reference/HyperSpec/Body/"
  "The root of the Common Lisp HyperSpec URL.
If you copy the HyperSpec to your local system, set this variable to
something like \"file:///usr/local/doc/HyperSpec/\".")

(def-albert-setting '("albert" "presentation" "output-dir") "Generated-Docs/"
  "The default dir to write output.")

(def-albert-setting '("albert" "presentation" "formats") (list "docbook")
  "What format should be produced.")

(def-albert-setting '("albert" "docbook" "generate") "book"
  "Generate 'book' for Docbook.")

(def-albert-setting '("albert" "docbook" "dtd") "/usr/lib/sgml/dtd/docbook-xml/4.1.2/docbookx.dtd"
  "The default place for the DocBook DTD.")

(def-albert-setting '("albert" "docbook" "baseurl") ""
  "Base URL to use in HTML output of DocBook documentation.")

(def-albert-setting '("albert" "docbook" "cvs-viewurl") ""
  "URL for a CVS-view program that can display a file.")

(def-albert-setting '("albert" "docbook" "cvs-tag") "HEAD"
  "Tag used when accessing cvsview packages, HEAD is default and refers to latest version.")

(def-albert-setting '("albert" "docbook" "textcolor") "black"
  "Default colour for text/fonts in DocBook documentation.")

(def-albert-setting '("albert" "docbook" "bgcolor") "white"
  "Background-colour to use in HTML output of DocBook documentation.")

(def-albert-setting '("albert" "docbook" "output-dir") nil
  "Outputdir for docbook-output, if NIL it uses the value of (albert presentation output-dir).")

(def-albert-setting '("system" "name") "Unknown Project"
  "Name of system/project being documented.")

(def-albert-setting '("system" "directory") ""
  "Path to the loaded system.")

(def-albert-setting '("system" "author" "name") "Unknown Author"
  "Name of author for system/project being documented.")

(def-albert-setting '("system" "author" "email") ""
  "Email of author for system/project being documented.")

(def-albert-setting '("system" "maintainer" "name") ""
  "Name of maintainer for system/project being documented, if empty or NIL author is assumed.")

(def-albert-setting '("system" "maintainer" "email") ""
  "Email of maintainer for system/project being documented.")

(def-albert-setting '("system" "version" "number") "v1.0"
  "Version-number for system/project being documented, can be a string.")

(def-albert-setting '("system" "date") nil
  "Date to put in documentation for project.")

(def-albert-setting '("system" "licence" "name") "Unknown Licence"
  "Name of licence used for system/project being documented.")

(def-albert-setting '("system" "licencefile") nil
  "Filename to the licencefile that should be inluded verbatim as legal notice.")

(def-albert-setting '("system" "description") ""
  "Description of the system.")

(def-albert-setting '("albert" "presentation" "index" "class-hierarchy") 8
  "Include a class-hierarchy/index in the documentation.  If this value is
an integer it refers to the number of classes needed to make a hierarchy. ")

(def-albert-setting '("albert" "presentation" "index" "global-index") t
  "Include a global index in the documentation.")

(def-albert-setting '("albert" "presentation" "class" "related-methods") nil
  "Include a list of 'related methods' (ie methods that dispatch on that class)
in the class description.  Still has issues and is fairly slow.  Might later become default.")

(def-albert-setting '("albert" "presentation" "class" "quickindex") 100
  "Minimum number of entries in a class before albert makes a quickindex.")

(def-albert-setting '("albert" "presentation" "gf" "separatepage") 2
  "How many methods does a generic function need to get a separate page.")

(def-albert-setting '("albert" "presentation" "variables" "separatepage") 5
  "How many variables in a package 'full listing' is needed to make a separate page for variables.")

(def-albert-setting '("albert" "presentation" "only-exported") nil
  "Will only exported methods and variables be presented?")

(def-albert-setting '("albert" "presentation" "funcallable" "calls") t
  "Will all relevant objects present a 'calls/calling' table?")

(def-albert-setting '("albert" "presentation" "funcallable" "calls-num") 12
  "How many calls are needed before a table is generated?")

(def-albert-setting '("albert" "presentation" "funcallable" "calledby") nil
  "Will all funcallable objects get a 'called by' table?")

(def-albert-setting '("albert" "verbose") nil
  "Should Albert be verbose or not. NIL or T value.")

(def-albert-setting '("albert" "lisp2csf" "display-progress") nil
  "Should Albert's Lisp2csf display how it progresses? NIL or T value.")

(def-albert-setting '("albert" "submarine-quiet") nil
  "Should Albert try not to say anything at all.")

(def-albert-setting '("albert" "lisp2csf" "accessors" "package") nil
  "Include accessors/readers/writers for a class in the package content list, useful for
GF-grouping, but might be unnecessary duplication.")

(def-albert-setting '("albert" "lisp2csf" "accessors" "class") t
  "Include accessors/readers/writers for a class in the class content list, it's usually
the place you look for accessor info, so this one is recommended unless you don't want it there.")


(def-albert-setting '("albert" "presentation" "trace-rules") nil
  "Should presentation-rules be traced?.")

(def-albert-setting '("albert" "presentation" "to-current-dir") nil
  "Should presentation paths be relative to the current directory (ie where albert is being run from) or as default use the directory of the system.")

(def-albert-setting '("albert" "presentation" "default-purpose-string" "class") "class"
  "If no @purpose field is found in class-documentation, what will the default be.")

(def-albert-setting '("albert" "presentation" "default-purpose-string" "struct") "struct"
  "If no @purpose field is found in struct-documentation, what will the default be.")

(def-albert-setting '("albert" "presentation" "default-purpose-string" "generic") "generic function"
  "If no @purpose field is found in gf-documentation, what will the default be.")

(def-albert-setting '("albert" "presentation" "default-purpose-string" "method") "method"
  "If no @purpose field is found in method-documentation, what will the default be.")

(def-albert-setting '("albert" "presentation" "legalnotice" "title") "Legal Notice"
  "Title for the 'legalnotice' section, if there is one.")
