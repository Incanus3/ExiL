(in-package :exil-user)

;; this functionality is not implemented yet, but this is the inteded usage

(complete-reset)

(deffacts genders
  (male john)
  (male george)
  (male steve)
  (female jane)
  (female marie)
  (female anne))

(deffacts relationships
  (parent-of john george)
  (perent-of george steve)
  (parent-of jane george)
  (parent-of john marie)
  (parent-of jane marie))

(defrule mother
  (female ?mother)
  (parent-of ?mother ?child)
  =>
  (mother-of ?mother ?child))

(defrule father
  (male ?father)
  (parent-of ?father ?child)
  =>
  (mother-of ?father ?child))

(defrule grandparent
  (parent-of ?grandparent ?parent)
  (parent-of ?parent ?person)
  =>
  (grandparent-of ?grandparent ?person))

(defrule direct-ancestor
  (parent-of ?ancestor ?person)
  =>
  (ancestor-of ?ancestor ?person))

(defrule indirect-ancestor
  (parent-of ?ancestor ?middleman)
  (ancestor-of ?middleman ?person)
  =>
  (ancestor-of ?ancestor ?person))

(defrule common-father
  (father-of ?father ?person)
  (father-of ?father ?sibling)
  =>
  (common-father ?person ?sibling))

(defrule common-mother
  (mother-of ?mother ?person)
  (mother-of ?mother ?sibling)
  =>
  (common-mother ?person ?sibling))

(defrule sibling
  (common-father ?person ?sibling)
  (common-mother ?person ?sibling)
  =>
  (sibling ?person ?sibling))

(reset)

(defgoal (mother-of ?mother george))

(goals) ; => ((mother-of ?mother george))
(back-step)
(goals) ; => ((female ?mother) (parent-of ?mother george))
(back-step)
(goals) ; => ((female ?mother) (parent-of ?mother george))
(back-step)
; (female ?mother) satisfied by (female jane)
(goals) ; => ((parent-of jane george))
(back-step)
; (parent-of jane george) satisfied by (parent-of jane george)
(goals) ; => ()

;(defgoal (father-of ?father george))
;(defgoal (grandparent-of ?grandparent george))
;(defgoal (sibling george ?sibling))
