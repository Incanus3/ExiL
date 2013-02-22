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
  (progn
    (princ (subseq (exil-env:facts *current-environment*)
                   (1- start-index)
                   (min end-index (+ start-index at-most -1))))
    nil))

(defun assert% (fact-spec)
  (add-fact *current-environment* (parse-fact *current-environment* fact-spec)))

; public
(defmacro assert (&rest fact-specs)
  "add facts to working memory"
  `(mapc #'assert% ',fact-specs))

;; retract needs to compute the facts to remove first, for when facts are
;; specified by indices and one is removed, the other indices shift
;; TODO: domluvit s dostalem, jestli je podpora specifikace faktu cislem
;; zadouci, pripadne odstranit podporu pro integerovy fact-spec
(defun retract% (fact-specs)
  (let (facts-to-remove)
    (dolist (fact-spec fact-specs)
      (typecase fact-spec
        (list (pushnew (parse-fact *current-environment* fact-spec)
                       facts-to-remove))
        (integer (pushnew (nth (1- fact-spec)
                               (exil-env:facts *current-environment*))
                          facts-to-remove))
        (t (error "~%Don't know how to retract ~A." fact-spec))))
    (dolist (fact facts-to-remove)
      (rem-fact *current-environment* fact))))

; retract supports either full fact specification e.g. (retract (is-animal duck))
; or number indices (starting with 1) for clips compatitibity.
; It can't support * to retract all facts as clips does, cause this symbol has
; a special meaning in lisp. retract-all does this instead.
; public
(defmacro retract (&rest fact-specs)
  "remove facts from working memory"
  `(retract% ',fact-specs))

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
(defun modify% (fact-spec mod-list)
  (let* ((old-fact (parse-fact *current-environment* fact-spec))
         (new-fact (modify-fact old-fact mod-list)))
    (rem-fact *current-environment* old-fact)
    (add-fact *current-environment* new-fact)))

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
  `(modify% ',fact-spec ',mod-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fact groups

; public
(defmacro deffacts (name &body fact-specs)
  "create group of facts to be asserted after (reset)"
  `(add-fact-group *current-environment* ',name
                   (parse-fact-group *current-environment* ',fact-specs)))

; public
(defmacro undeffacts (name)
  "delete fact group"
  `(rem-fact-group *current-environment* ',name))
