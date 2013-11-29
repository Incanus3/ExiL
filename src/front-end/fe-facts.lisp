(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; facts

;; TODO: domluvit s dostalem, jestli je podpora specifikace faktu cislem
;; zadouci, pripadne pridat jinou moznost filtrovani faktu, napr. podle
;; templatu, nebo patternu
; public
(defun facts (&optional (start-index 1)
                (end-index (length (exil-env:facts *current-environment*)))
                (at-most end-index))
  "return at most at-most facts from start-index to end-index"
  (mapcar #'external (subseq (exil-env:facts *current-environment*)
                             (1- start-index)
                             (min end-index (+ start-index at-most -1)))))

(defun assert% (fact-spec)
  (add-fact *current-environment* (parse-fact *current-environment* fact-spec)
	    (format nil "(assert ~S)" fact-spec)))

(defun assertf (&rest fact-specs)
  (mapc #'assert% fact-specs)
  nil)

(defun quote-each (list)
  (mapcar (lambda (spec) `(quote ,spec)) list))

; public
(defmacro assert (&rest fact-specs)
  "add facts to working memory"
  `(assertf ,@(quote-each fact-specs)))

;; retract needs to compute the facts to remove first, for when facts are
;; specified by indices and one is removed, the other indices shift
;; TODO: domluvit s dostalem, jestli je podpora specifikace faktu cislem
;; zadouci, pripadne odstranit podporu pro integerovy fact-spec
;; TODO: check for fact index out of range
(defun retractf (&rest fact-specs)
  (let (facts-to-remove)
    (dolist (fact-spec fact-specs)
      (typecase fact-spec
        (list (push (cons fact-spec (parse-fact *current-environment* fact-spec))
                    facts-to-remove))
        (integer (push (cons fact-spec (nth (1- fact-spec)
					    (exil-env:facts *current-environment*)))
                       facts-to-remove))
        (t (error "~%Don't know how to retract ~A." fact-spec))))
    (iter (for (spec . fact) :in (nreverse facts-to-remove))
	  (rem-fact *current-environment* fact (format nil "(retract ~A)" spec)))))

; retract supports either full fact specification e.g. (retract (is-animal duck))
; or number indices (starting with 1) for clips compatitibity.
; It can't support * to retract all facts as clips does, cause this symbol has
; a special meaning in lisp. retract-all does this instead.
; public
(defmacro retract (&rest fact-specs)
  "remove facts from working memory"
  `(retractf ,@(quote-each fact-specs)))

; public
(defun retract-all ()
  "remove all facts from working memory"
  (clear-env *current-environment*))

;; mod-list is a mapping from slot-name to new value
;; it can be either plist for non-clips syntax of alist for clips syntax
;; TODO: modify-fact should be responsibility of parser, as it works with
;; external representation of mod-list
;; this should ask parser to parse fact-spec, then to modify the fact
;; than remove the old fact from environment and add the modified one
(defun modifyf (fact-spec mod-list)
  (let* ((old-fact (parse-fact *current-environment* fact-spec))
         (new-fact (modify-fact old-fact mod-list)))
    (mod-fact *current-environment* old-fact new-fact)))

;; used as follows:
;; (defrule push
;;   (goal :object ?x :from ?y :to ?z)
;;   ?object <- (in :object ?x :location ?y)
;;   ?robot <- (in :object robot :location ?y)
;;   =>
;;   (modify ?robot :location ?z)
;;   (modify ?object :location ?z))
;; modify works for template-facts ONLY!
;; it doesn't make much sense to use it for simple facts as there are no
;; slot names that could be used in the mod-list
;; CLIPS doesn't support modify for simple-facts either
; public
(defmacro modify (fact-spec &rest mod-list)
  "modify fact-spec by mod-list"
  `(modifyf ',fact-spec ',mod-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fact groups

(defun deffactsf (name fact-specs)
  (add-fact-group *current-environment* name
		  (parse-fact-group *current-environment* fact-specs)
		  (format nil "(deffacts ~A ~A)" name fact-specs)))

; public
(defmacro deffacts (name &body fact-specs)
  "create group of facts to be asserted after (reset)"
  `(deffactsf ',name ',fact-specs))

(defun undeffactsf (name)
  (rem-fact-group *current-environment* name
		  (format nil "(undeffacts ~A)" name)))

; public
(defmacro undeffacts (name)
  "delete fact group"
  `(undeffactsf ',name))

(defun fact-groups ()
  (fact-group-names *current-environment*))

;; this isn't a great solution - front-end shouldn't be responsible for
;; creating fact-group's external representation
;; fact-group should probably be an object (even though only a simple wrapper)
;; so that we can define methods for it
(defun find-fact-group (name)
  (let ((facts (eenv:find-fact-group *current-environment* name)))
    (when facts
      (cons (to-keyword name) (mapcar #'external facts)))))
