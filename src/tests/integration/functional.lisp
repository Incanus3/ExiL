(in-package :integration-tests)
(declaim (optimize (compilation-speed 0) (debug 3) (space 0) (speed 0)))

(defclass functional-integration-tests (test-case)
  ((env :reader env :initform exil::*current-environment*)))

;; all front-end macros, that take symbolic representation of ExiL entities
;; (facts, patterns, templates, rules, goals, watcher names, environment names,
;; strategies), should have functional counterparts that take these expressions
;; (symbols, lists) as arguments so that other code can retrieve, manipulate,
;; generate, etc. the entities' sybmolic representation
;; e.g. when you write a function, that returns the list '(in box hall)
;; there's currently no way to assert this as a fact => the macros are only
;; useful for interactive usage, not programmatic
;; defenvf, setenvf
;; deftemplatef
;; assertf, retractf, modifyf
;; deffactsf undeffactsf
;; defrulef, undefrulef
;; defgoalf, undefgoalf
;; defstrategyf, setstrategyf
;; watchf, unwatchf, watchedpf

;; there should also be a complete set of query functions, that return this
;; symbolic representation (instead of just printing it):
;; environments - just names
;; templates - just names
;; find-template - specifier
;; facts - specifiers
;; fact-groups - just names
;; find-fact-group - specifier
;; strategies - just names
;; find-strategy - function
;; rules - just names
;; find-rule - specifier
;; goals - specifiers

;; I'll suffix these functional counterparts with f for now, e.g. deftemplatef

;; what to do with rule condition ?fact <- (pattern) notation?

(defmethod set-up ((tests functional-integration-tests))
  (complete-reset))

(def-test-method functional-integration-test ((tests functional-integration-tests) :run t)
  (reset)
  (run)

  (with-slots (env) tests
    (deftemplatef :goal '((action :default run) object from to))
    (assert-equal (find-template :goal)
                  '(:goal ((:action :default run) (:object) (:from) (:to))))

    (deftemplatef :in '(object location))

    (assert-equal (templates) '(:goal :in))

    ;; (deffactsf :world
    ;;     '((in :object robot :location A)
    ;;       (in :object box :location B)
    ;;       (goal :action push :object box :from B :to A)))

    ;; (defrulef :move
    ;;     '((goal :action push :object ?x :from ?y)
    ;;       (in :object ?x :location ?y)
    ;;       (- in :object robot :location ?y)
    ;;       ?robot <- (in :object robot :location ?))
    ;;   '((modify ?robot :location ?y)))

    ;; (defrulef :push
    ;;     '((goal :action push :object ?x :from ?y :to ?z)
    ;;       ?object <- (in :object ?x :location ?y)
    ;;       ?robot <- (in :object robot :location ?y))
    ;;   '((modify ?robot :location ?z)
    ;;     (modify ?object :location ?z)))

    ;; (defrulef :stop
    ;;     '((goal :action push :object ?x :to ?y)
    ;;       (in :object ?x :location ?y))
    ;;   '((halt)))

    ;; (unwatchf :all)    ;; (let ((in-template (eenv::find-template env :in)))
    ;; ;;   (assert-true (eenv::find-fact env (eenv::make-template-fact
    ;; ;;     				 in-template '(:object box :location A)))))
    ;; (assert-true (find '(in :object box :location A) (facts)))
    ))

(add-test-suite 'functional-integration-tests)
;;(textui-test-run (get-suite functional-integration-tests))
