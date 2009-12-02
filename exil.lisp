(in-package :exil)

(defvar *facts* nil)
(defvar *templates* nil)
(defvar *rules* nil)

(defmacro deftemplate (name fileds)
  "Define rule template"

  )

(defmacro assert (fact)
  "Add fact into working memory"

  )

(defmacro retract (fact)
  "Remove fact from working memory"

  )

(defmacro deffacts (facts-list)
  "Create group of facts to be asserted after (reset)"

  )

(defmacro defrule (rule)
  "Define rule"

  )

(defun reset ()
  "Reset the environment"

  )

(defun run ()
  "Run the infenece engine"

  )
