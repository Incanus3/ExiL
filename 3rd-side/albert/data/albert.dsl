<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
  <!-- HTML by default -->
  <!ENTITY % html "INCLUDE">
  <!ENTITY % print "IGNORE">

  <![%print;[
    <!ENTITY % html "IGNORE">
    <!ENTITY docbook.dsl PUBLIC 
        "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN" CDATA DSSSL>
  ]]>

  <![%html;[
     <!ENTITY docbook.dsl PUBLIC
        "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA DSSSL>
  ]]>
]>

<style-sheet>

<!-- HTML tweaking starts here -->
<style-specification id="html" use="docbook">
<style-specification-body>

(define %albert-version% "0.4.9")

(define %css-decoration%
  ;; Enable CSS decoration of elements
  #t)

(define %stylesheet%
  ;; Name of the stylesheet to use
  "albert.css")

(define $generate-chapter-toc$ 
  ;; Should a Chapter Table of Contents be produced?
  (lambda ()
    #f))

(define %always-format-variablelist-as-table%
  ;; Always format VariableLists as tables?
  #t)

(define %chapter-autolabel% 
  ;; Are chapters enumerated?
  #f)

(define %gentext-nav-use-tables%
    ;; Use tables to build the navigation headers and footers?
    #t)

(define %root-filename%
    ;; Name for the root HTML document
    "index")

(define %use-id-as-filename%
  ;; Use ID attributes as name for component HTML files?
  #t)

(define %html-ext% 
  ;; Default extension for HTML output files
  ".html")

(define %shade-verbatim%  
  ;; Should verbatim environments be shaded?
  #t)

(define %section-autolabel%
  ;; Are sections enumerated?
  #f)

;; no horisontal ruler to separate
(define (book-titlepage-separator side)
  (empty-sosofo))


(define %spacing-paras%
  ;; Should extraneous "P" tags be output to force the correct vertical
  ;; spacing around things like tables.  This is ugly because different
  ;; browsers do different things.  Turning this one can also create
  ;; illegal HTML.
  #f)

;; Important:
;; here we turn off the default navigation (recommended)
(define %header-navigation% #f)
(define %footer-navigation% #f)

(define %generate-legalnotice-link%
    ;; Should the legalnotice be included in a separate file that
    ;; is linked to, if so go for #t, if you want it included verbatim
    ;; on the titlepage, go for #f  Default is to put it in a
    ;; separate file, as it would allow using full licences.
    #t)

;; Important
(define %albert-header-navigation%
    ;; will we use albert navigation in the header? (recommended)
    #t)

(define %albert-footer-navigation%
    ;; will we use albert navigation in the header? (recommended)
    #t)


(define %albert-text-navigation%
    ;; Will the little text-line saying what is next, prev, and up be used?
    #t)

(define %albert-navigation-ruler%
    ;; Use a ruler to separate navigation areas from the rest of the text
    #f)

(define %albert-navigation-iconpath%
    ;; "Defines the url/path to the icons, can be relative
    "icons/")

(define %albert-use-icons-for-export-status%
    ;; If true, uses icons to show export-status
    #t)
    
;;; ====== Code =========

(define (albert-nav-up? node)
    (nav-up? node)) ;; already in nwalsh
  
(define (albert-nav-up elemnode nav-style)
    (let* ((up      (parent elemnode))
	   (up-href (inherited-dbhtml-value elemnode "up-href"))
	   (uplink? (not (or (node-list-empty? up)
			     (node-list=? up (sgml-root-element)))))
	   (href    (if up-href
			up-href
			(if uplink?
			    (href-to up)
			    #f))))
      (cond ((equal? nav-style 'image)
	     (make element gi: "a"
		   attributes: (list
				(list "href" href)
				(list "title" (element-title-string up))
				(list "accesskey" "U"))
		   (make element gi: "img"
			 attributes: (list
				      (list "src" (string-append %albert-navigation-iconpath%
								 "gnome_up.png"))
				      (list "border" "0"))
			 (empty-sosofo))))
	    
	    ((equal? nav-style 'text)
	     (make sequence
		 (make element gi: "b"
		       attributes: (list
				    (list "class" "navlabel"))
		       (literal "Up:"))
		 (literal " ")
		 (make element gi: "a"
		       attributes: (list
				    (list "class" "sectref")
				    (list "href" href))
		       (element-title-sosofo up))))

	    (else
	     (empty-sosofo))
	    )))


(define (albert-nav-prev? node)
    (not (node-list-empty? node)))

(define (albert-nav-prev node nav-style)
    (cond ((equal? nav-style 'image)
	   (make element gi: "a"
		 attributes: (list
			      (list "href" (href-to node))
			      (list "title" (element-title-string node))
			      (list "accesskey" "p"))
		 (make element gi: "img"
		       attributes: (list
				    (list "src" (string-append %albert-navigation-iconpath%
							       "gnome_left.png"))
				    (list "border" "0"))
		       (empty-sosofo))
		 ))
	  ((equal? nav-style 'text)
	   (make sequence
		 (make element gi: "b"
		       attributes: (list
				    (list "class" "navlabel"))
		       (literal "Previous:"))
		 (literal " ")
		 (make element gi: "a"
		       attributes: (list
				    (list "class" "sectref")
				    (list "href" (href-to node)))
		       (element-title-sosofo node))
		 ))
	  (else
	   (empty-sosofo))
	  ))

(define (albert-nav-next? node)
    (not (node-list-empty? node)))

(define (albert-nav-next node nav-style)
    (cond ((equal? nav-style 'image)
	   (make element gi: "a"
		 attributes: (list
			      (list "href" (href-to node))
			      (list "title" (element-title-string node))
			      (list "accesskey" "N"))
		 (make element gi: "img"
		       attributes: (list
				    (list "src" (string-append %albert-navigation-iconpath%
							       "gnome_right.png"))
				    (list "border" "0"))
		       (empty-sosofo))))
	  
	 ((equal? nav-style 'text)
	   (make sequence
		 (make element gi: "b"
		       attributes: (list
				    (list "class" "navlabel"))
		       (literal "Next:"))
		 (literal " ")
		 (make element gi: "a"
		       attributes: (list
				    (list "class" "sectref")
				    (list "href" (href-to node)))
		       (element-title-sosofo node))
		 ))
	 (else
	  (empty-sosofo))))

(define ($user-header-navigation$ #!optional 
                                  (prev (empty-node-list))
                                  (next (empty-node-list))
                                  (prevm (empty-node-list))
                                  (nextm (empty-node-list)))
    (if %albert-header-navigation%
	(albert-navigation 'header prev next prevm nextm)
	(empty-sosofo)))

(define ($user-footer-navigation$ #!optional 
                                  (prev (empty-node-list))
                                  (next (empty-node-list))
                                  (prevm (empty-node-list))
                                  (nextm (empty-node-list)))
    (if %albert-footer-navigation%
	(albert-navigation 'footer prev next prevm nextm)
	(empty-sosofo)))

(define (albert-get-book-element cur-node)
    (if (equal? (gi cur-node) (normalize "book"))
	cur-node
	(albert-get-book-element (parent cur-node))))
    

(define (albert-get-book-title cur-node)
    (element-title-string (albert-get-book-element cur-node)))

(define (albert-get-pubdate cur-node)
    (let* ((bookinfo (select-elements (children (albert-get-book-element cur-node)) "bookinfo"))
	   (pubdate (select-elements (children bookinfo) "pubdate")))
      (process-node-list pubdate)))

(define (albert-navigation-image nav-style prev next prevm nextm)
	  (make element gi: "table"
		attributes: (list
			     (list "align" "center")
			     (list "width" "100%")
			     (list "cellpadding" "0")
			     (list "cellspacing" "2"))
		
		(make element gi: "tr"
		      (make sequence
			    
			    (make element gi: "td"
				  (if (albert-nav-prev? prev)
				      (albert-nav-prev prev 'image)
				      (make entity-ref name: "nbsp")))
			     
			    (make element gi: "td"
 				  (if (albert-nav-up? (current-node))
 				      (albert-nav-up (current-node) 'image)
 				      (make entity-ref name: "nbsp")))
			    
 			     (make element gi: "td"
 				   (if (albert-nav-next? next)
 				       (albert-nav-next next 'image)
 				       (make entity-ref name: "nbsp")))
			     
			    (make element gi: "td"
				  attributes: (list
					       (list "align" "center")
					       (list "width" "100%"))
				  (make element gi: "p"
					(make sequence
					      ;;(literal %albert-doc-header%)
					      (literal (albert-get-book-title (current-node)))
					      )))

			    (make element gi: "td"
				  (make element gi: "a"
					attributes: (list
						     (list "title" "Table of Contents")
						     (list "href" "index.html")
						     (list "accesskey" "H"))
					
					(make element gi: "img"
					      attributes: (list
							   (list "src" (string-append %albert-navigation-iconpath%
										      "gnome_home.png"))
							   (list "border" "0"))
					      (empty-sosofo))
						  
					    
					    ;;(gentext-nav-next next)
					    ))
			    
			    (make element gi: "td"
				  (make element gi: "a"
					attributes: (list
						     (list "title" "Global Index")
						     (list "href" "globalindex.html")
						     (list "accesskey" "I"))
					
					(make element gi: "img"
					      attributes: (list
							   (list "src" (string-append %albert-navigation-iconpath%
										      "gnome_index.png"))
							   (list "border" "0"))
					      (empty-sosofo))
						  
					    
					    ;;(gentext-nav-next next)
					    ))
			    

			    ))
		))

(define (albert-navigation-text nav-style prev next prevm nextm)
    (make sequence
	  (if (albert-nav-prev? prev)
	      (albert-nav-prev prev 'text)
	      (empty-sosofo))
	  
	  (literal " ")
	  
	  (if (albert-nav-up? (current-node))
	      (albert-nav-up (current-node) 'text)
	      (empty-sosofo))
	  
	  (literal " ")
	  
	  (if (albert-nav-next? next)
	      (albert-nav-next next 'text)
	      (empty-sosofo))))

(define (albert-navigation nav-style prev next prevm nextm)
    (make element gi: "div"
	  attributes: (list
		       (list "class" "navigation"))
	  (cond ((equal? nav-style 'header)
		 (make sequence
		       (albert-navigation-image nav-style prev next prevm nextm)
		       (cond (%albert-text-navigation%
			      (albert-navigation-text nav-style prev next prevm nextm))
			     (else
			      (empty-sosofo)))
		       (if %albert-navigation-ruler%
			   (make element gi: "hr"
				 attributes: '(("class" "navigationruler")
					       ("align" "center")
					       ("width" "80%"))
				 (empty-sosofo))
			   (empty-sosofo))
		       ))
		
		((equal? nav-style 'footer)
		 (make sequence
		       (if %albert-navigation-ruler%
			   (make element gi: "hr"
				 attributes: '(("align" "center")
					       ("width" "80%"))
				 (empty-sosofo))
			   (empty-sosofo))
		       
		       (cond (%albert-text-navigation%
			      (albert-navigation-text nav-style prev next prevm nextm))
			     (else
			      (empty-sosofo)))
		       
		       (albert-navigation-image nav-style prev next prevm nextm)
		       ))
		(else
		 (empty-sosofo)))
	  ))


;; Albert: this one makes a small note at the bottom that the page was generated by Albert
;;         You can also define $html-body-start$, $html-body-content-start$, $html-body-content-end$

(define ($html-body-end$)
    (make sequence
	  (make element gi: "p"
		attributes: (list
			     (list "class" "bottom")
			     (list "align" "left"))
		(make sequence
		      
		      (literal "This documentation was generated ")
		      (albert-get-pubdate (current-node))
		      (literal " from the original sources by ")
		      
		      (make element gi: "a"
			    attributes: (list (list "href" "http://albert.sourceforge.net/"))
			    (make sequence (literal "Albert")))
		      (literal " v.")
		      (literal %albert-version%)
		      (literal ".")
		      ))))


;; I want to change this so that for programlisting we use a blue background,
;; but for example or informalexample we use green.
(define $shade-verbatim-attr$
  ;; Attributes used to create a shaded verbatim environment.
  (lambda ()
    (if (or (equal? (gi (parent (current-node)))
		    (normalize "example"))
	    (equal? (gi (parent (current-node)))
		    (normalize "informalexample")))
	(list
	 (list "class" (gi (parent (current-node))))
	 (list "border" "0")
	 (list "bgcolor" "#d8f8d8")
	 (list "width" "100%")
	 (list "cellpadding" "6"))
	(list
	 (list "class" (gi (parent (current-node))))
	 (list "border" "0")
	 (list "bgcolor" "#aaddaa") ;;"#D6E8FF")
	 (list "width" "100%")
	 (list "cellpadding" "6"))
	)
  )
)


;; These are some customizations to the standard HTML output produced by the
;; Modular DocBook Stylesheets.
;; They have been adopted from gtk-doc 0.3

;; Albert: do I want this?

;; This overrides the tgroup definition (copied from 1.20, dbtable.dsl).
;; It changes the table background color, cell spacing and cell padding.
;; remove later=
(element tgroup
  (let* ((wrapper   (parent (current-node)))
	 (frameattr (attribute-string (normalize "frame") wrapper))
	 (pgwide    (attribute-string (normalize "pgwide") wrapper))
	 (footnotes (select-elements (descendants (current-node)) 
				     (normalize "footnote")))
	 (border (if (equal? frameattr (normalize "none"))
		     '(("border" "0"))
		     '(("border" "1"))))
	 (roleattr (attribute-string (normalize "role") wrapper))

	 ;; FIXME: I thought that I should use normalize ("params") etc. here,
	 ;; but that doesn't work. Anyone know why?
	 (bgcolor (cond ((equal? roleattr "params")
			 '(("bgcolor" "#ffd0d0")))
			((equal? roleattr "classfacts")
			 '(("bgcolor" "#cdba96")))
			((equal? roleattr "struct")
			 '(("bgcolor" "#fff0d0")))
			((equal? roleattr "enum")
			 '(("bgcolor" "#f0f0d0")))
			(else
			 '(("bgcolor" "#ffffff")))))

	 (width (if (equal? pgwide "1")
		    (list (list "width" ($table-width$)))
		    '()))
	 (head (select-elements (children (current-node)) (normalize "thead")))
	 (body (select-elements (children (current-node)) (normalize "tbody")))
	 (feet (select-elements (children (current-node)) (normalize "tfoot"))))
    (make element gi: "table"
	  attributes: (append
		       border
		       width
		       bgcolor
		       '(("cellspacing" "0"))
		       '(("cellpadding" "4"))
		       (if %cals-table-class%
			   (list (list "class" %cals-table-class%))
			   '()))
	  (process-node-list head)
	  (process-node-list body)
	  (process-node-list feet)
	  (make-table-endnotes))))


;; Albert: Tweaked this one slightly, dunno what the GTKDOCLINK is for anymore

;; For hypertext links for which no target is found in the document, we output
;; our own special tag which we use later to resolve cross-document links.
;; needs tweaking
(element link 
  (let* ((target (element-with-id (attribute-string (normalize "linkend"))))
	 (hovertext (attribute-string (normalize "hovertext"))))
    (if (node-list-empty? target)
	(process-children)
	(make element gi: "a"
	      attributes: (append
			   (list (list "href" (href-to target)))
			   (if hovertext
			       (list (list "title" hovertext))
			       '()))
	      (process-children)))))

(element ulink
	 (let ((attrs (list
		       (list "href" (attribute-string (normalize "url")))
		       (list "target" "_top")
		       (if (equal? "clhs" (attribute-string (normalize "type")))
			   (list "class" "hyperspec")
			   (list "class" "other"))
		       (list "title" (if (equal? "clhs" (attribute-string (normalize "type")))
					 "Hyperspec Entry, package COMMON-LISP"
					 (attribute-string (normalize "url")))))))
	   (make element gi: "a"
		 attributes: attrs
		 (if (node-list-empty? (children (current-node)))
		     (literal (attribute-string (normalize "url")))
		     (process-children)))))




;; Albert: I think we want this one, but I'm not sure it is 100% correct

;; hackish, we want our special ones in green
(element (refsect1 title)
  (let ((the-class (attribute-string (normalize "class"))))
    (if (equal? the-class "contenttitle")
	(make element gi: "h2"
	      attributes: (list
			   (list "class" (attribute-string (normalize "class"))))
	      (process-children))
	;; default in the included stylesheets
	($lowtitle$ 2))))

;; Returns code for export-status icon, or an emptysosofo if EXPORTED? is #F

(define (possible-export-icon exported?)
    (if %albert-use-icons-for-export-status%
	(make sequence
	      (make element gi: "a"
	      attributes: (list
			   (list "title" (if exported?
					     "Exported from package"
					     "Internal to package")))
	      (make element gi: "img"
		    attributes: (list
				 (list "src" (string-append %albert-navigation-iconpath%
							    (if exported?
								"exported.png"
								"internal.png")))
				 (list "border" "0"))
		    (empty-sosofo)))
	      (literal " "))
	
	(empty-sosofo)))

;; Albert: No idea what this does
;;   suggestion: this seems to print the title of a method above the programlisting
;;               in other words the 'title' in a formalpara
(define ($runinhead$)
    (let* ((title-parent (parent (current-node)))
	   (parent-class (attribute-string (normalize "class") title-parent))
	   (exported? (string? parent-class))
	   (cont-class (if exported?
			   (list "class" "exported")
			   (list "class" "not-exported")))) ;; bad
      (make sequence
	    
	    (possible-export-icon exported?)
	    
	    (make element gi: "b"
		  attributes: (list
			       (list "class" "minorheader"))
		  (make element gi: "font"
			attributes: (list (list "size" "+1")
			      cont-class
			      ;;(list "jigsaw" parent-class)
			      ;;(list "COLOR" "darkgreen")
			      )
			(process-children)))
	    )))

(element (programlisting property)
   (let* ((exported? (equal? "exported" (attribute-string (normalize "role"))))
	  (cont-class (if exported?
			  (list "class" "exported")
			  (list "class" "not-exported"))))
     (make sequence
	   (possible-export-icon exported?)
	   (make element gi: "font"
		 attributes: (list cont-class)
		 (process-children)))))



;; Albert: This is kindof experimental, we need to mark where a refentry/etc
;;         is coming from to help CSS

(define ($refentry-body$)
  (let ((id (element-id (current-node)))
	(the-class (attribute-string (normalize "class"))))
    (make sequence 
      (make element gi: "h1"
	    attributes:
	    (append
	     (if (equal? the-class "defclass")
		 (list (list "class" "defclass"))
		 '()))
            (make sequence
              (make element gi: "a"
                    attributes: (list (list "name" id))
                    (empty-sosofo))
              (element-title-sosofo (current-node))))
      (process-children))))

(element refentry
  (html-document (with-mode refentry-head-title-mode
                   (literal (element-title-string (current-node))))
                 ($refentry-body$)))

; ;; Albert: Do I want this?  I suspect this will be changed!!

; ;; Override the book declaration, so that we generate a crossreference
; ;; for the book

; (element book 
;   (let* ((bookinfo  (select-elements (children (current-node)) (normalize "bookinfo")))
; 	 (ititle   (select-elements (children bookinfo) (normalize "title")))
; 	 (title    (if (node-list-empty? ititle)
; 		       (select-elements (children (current-node)) (normalize "title"))
; 		       (node-list-first ititle)))
; 	 (nl       (titlepage-info-elements (current-node) bookinfo))
; 	 (tsosofo  (with-mode head-title-mode
; 		     (process-node-list title)))
; 	 (dedication (select-elements (children (current-node)) (normalize "dedication"))))
;     (make sequence
;      (html-document 
;       tsosofo
;       (make element gi: "DIV"
; 	    attributes: '(("CLASS" "BOOK"))
; 	    (if %generate-book-titlepage%
; 		(make sequence
; 		  (book-titlepage nl 'recto)
; 		  (book-titlepage nl 'verso))
; 		(empty-sosofo))
	    
; 	    (if (node-list-empty? dedication)
; 		(empty-sosofo)
; 		(with-mode dedication-page-mode
; 		  (process-node-list dedication)))
	    
; 	    (if (not (generate-toc-in-front))
; 		(process-children)
; 		(empty-sosofo))
	    
; 	    (if %generate-book-toc%
; 		(build-toc (current-node) (toc-depth (current-node)))
; 		(empty-sosofo))
	    
; 	    ;;	  (let loop ((gilist %generate-book-lot-list%))
; 	    ;;	    (if (null? gilist)
; 	    ;;		(empty-sosofo)
; 	    ;;		(if (not (node-list-empty? 
; 	    ;;			  (select-elements (descendants (current-node))
; 	    ;;					   (car gilist))))
; 	    ;;		    (make sequence
; 	    ;;		      (build-lot (current-node) (car gilist))
; 	    ;;		      (loop (cdr gilist)))
; 	    ;;		    (loop (cdr gilist)))))
	  
; 	    (if (generate-toc-in-front)
; 		(process-children)
; 		(empty-sosofo))))
;      (make entity 
;        system-id: "index.sgml"
;        (with-mode generate-index-mode
; 	 (process-children))))))

; ;; Albert: Do I need this?

; ;; Mode for generating cross references

; (define (process-child-elements)
;   (process-node-list
;    (node-list-map (lambda (snl)
;                     (if (equal? (node-property 'class-name snl) 'element)
;                         snl
;                         (empty-node-list)))
;                   (children (current-node)))))

; ;; Albert: This seems to be a side-effect of the stuff introduced in book
; ;;         above.. it seems to go through the tree and register stuff..
; ;;         it does no harm so it'll stay I guess

; (mode generate-index-mode
;   (element anchor
;     (if (attribute-string "href" (current-node))
; 	(empty-sosofo)
; 	(make formatting-instruction data:
; 	      (string-append "\less-than-sign;ANCHOR id =\""
; 			     (attribute-string "id" (current-node))
; 			     "\" href=\""
; 			     (href-to (current-node))
; 			     "\"\greater-than-sign;
; "))))

;   ;; We also want to be able to link to complete RefEntry.
;   (element refentry
;     (make sequence
;       (make formatting-instruction data:
; 	    (string-append "\less-than-sign;ANCHOR id =\""
; 			   (attribute-string "id" (current-node))
; 			   "\" href=\""
; 			   (href-to (current-node))
; 			   "\"\greater-than-sign;"))
;       (process-child-elements)))

;   (default
;     (process-child-elements)))


;; These override 2 functions which return the English text to use for links to
;; previous and next pages. (copied from 1.20, dbl1en.dsl).
;;(define (gentext-en-nav-prev prev) 
;;  (make sequence (literal "<<< Previous Page")))
;;(define (gentext-en-nav-next next)
;;  (make sequence (literal "Next Page >>>")))

;; This overrides the refsect2 definition (copied from 1.20, dbrfntry.dsl).
;; It puts a horizontal rule before each function/struct/... description,
;; except the first one in the refsect1.

;;(element refsect2
;;  (make sequence
;;    (if (first-sibling?)
;;	(empty-sosofo)
;;	(make empty-element gi: "HR"))
;;    ($block-container$)))


;;(define ($runinhead$)
;;    (let ((title (data (current-node))))
;;      (make element gi: "FONT"
;;	    attributes: (list (list "SIZE" "+1"))
;;	    (process-children))
;;  )


</style-specification-body>
</style-specification>

<!-- End HTML -->


<external-specification id="docbook" document="docbook.dsl">
</style-sheet>
