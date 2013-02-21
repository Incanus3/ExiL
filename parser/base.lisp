(in-package :exil-parser)

;; template- and generic fact and pattern makers - front-end template-object
;; specification parsing

;; functions that (potentially) create template objects are defined as methods
;; of environment, as they need access to environment's template list

(defgeneric parse-template (name slots)
  (:documentation "create template from external representation"))
;; used by assert
(defgeneric parse-fact (env fact-spec)
  (:documentation "create fact from external representation"))
(defgeneric modify-fact (fact mod-list)
  (:documentation "create new fact from fact and mod-list"))
;; used for creating patterns from rule's conditions
(defgeneric parse-pattern (env pattern-spec &key match-var)
  (:documentation "create pattern from external representation"))

(defgeneric parse-fact-group (env name fact-specs)
  (:documentation "create fact group from external representation"))

(defgeneric parse-rule (env name body)
  (:documentation "create rule from external representation"))
