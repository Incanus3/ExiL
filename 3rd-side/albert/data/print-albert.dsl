<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
  <!-- HTML by default -->
  <!ENTITY % html "IGNORE">
  <!ENTITY % print "INCLUDE">

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

<!-- Print tweaking starts here -->
<style-specification id="print" use="docbook">
<style-specification-body>

(define %generate-book-toc% 
  ;; Should a Table of Contents be produced for Books?
  #f)

(define %generate-book-titlepage%
  ;; Should a book title page be produced?
  #f)

(define ($generate-book-lot-list$)
    ;; Which Lists of Titles should be produced for Books?
    '()) ;; don't want any

(define %generate-part-titlepage% 
  ;; Should a part title page be produced?
  #f)

(define %generate-reference-titlepage% 
  ;; Should a reference title page be produced?
  #f)

;; this mimics html more
(define %refentry-new-page% 
  ;; 'RefEntry' starts on new page?
  #t)

;; don't bother with urls, really. 
(define %show-ulinks%
  ;; Display URLs after ULinks?
  #f)

;; usians might want something else
(define %paper-type%
  ;; Name of paper type
  "A4")

(define %two-side% 
  ;; Is two-sided output being produced?
  #f)

(define %bf-size%
  ;; Defines the body font size
  (case %visual-acuity%
    (("tiny") 8pt)
    (("normal") 10pt)
    (("presbyopic") 12pt)
    (("large-type") 24pt)))

(define %visual-acuity%
  ;; General measure of document text size
  ;; "presbyopic"
  ;; "large-type"
  ;; "normal"
  "tiny" ;; to get as much as possible in one page
  )

;;; === margins/factors:

;; most of these aim to put much info on each page, just
;; comment out stuff if you want wider settings/margins.

(define %head-before-factor% 
  ;; Factor used to calculate space above a title
  0.5) ; was 0.75

(define %left-margin% 
  ;; Width of left margin
  3pi)

(define %right-margin% 
  ;; Width of the right margin
  3pi)

(define %para-sep% 
  ;; Distance between paragraphs
  (/ %bf-size% 2.4))

(define %top-margin% 
  ;; Height of top margin
  (if (equal? %visual-acuity% "large-type")
      3.5pi
      2pi))

(define %header-margin% 
  ;; Height of header margin
  (if (equal? %visual-acuity% "large-type") 
      3.5pi 
      2pi))

(define %bottom-margin% 
  ;; Height of bottom margin
  (if (equal? %visual-acuity% "large-type")
      3.5pi 
      2pi))

;;;; === Seems to screw up things

;; Will crash jade badly :-)
;;(define %always-format-variablelist-as-table%
;;  ;; Always format VariableLists as tables?
;;  #t)


</style-specification-body>
</style-specification>

<!-- End Print -->


<external-specification id="docbook" document="docbook.dsl">
</style-sheet>
