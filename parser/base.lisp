(in-package :exil-parser)

;; template- and generic fact and pattern makers - front-end template-object
;; specification parsing

;; functions that (potentially) create template objects are defined as methods
;; of environment, as they need access to environment's template list

;; used by front-end:deftemplate
(defgeneric parse-template (name slots)
  (:documentation "create template from external representation"))
;; used by front-end:assert, retract and modify
(defgeneric parse-fact (env fact-spec)
  (:documentation "create fact from external representation"))
;; used by front-end:modify
(defgeneric modify-fact (fact mod-list)
  (:documentation "create new fact from fact and mod-list"))
;; used by parse-rule for creating patterns from rule's conditions
(defgeneric parse-pattern (env pattern-spec &key match-var)
  (:documentation "create pattern from external representation"))

(defgeneric parse-fact-group (env name fact-specs)
  (:documentation "create fact group from external representation"))
;; used by front-end:defrule
(defgeneric parse-rule (env name body)
  (:documentation "create rule from external representation"))
