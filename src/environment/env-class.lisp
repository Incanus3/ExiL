(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass environment ()
  ((watchers :accessor watchers
             :documentation "alist, (:facts, :rules, :activations) -> t/nil")
   (templates :accessor templates
              :documentation "hash table, assigns template instance to name")
   (facts :initform () :accessor facts
          :documentation "list of fact instances")
   (fact-groups :initform () :accessor fact-groups
                :documentation "((group-name description*)*)")
   (strategies :accessor strategies
               :documentation "alist, assigns strategy function to name symbol")
   (current-strategy-name :initform :depth-strategy
                          :accessor current-strategy-name
                          :documentation "symbol")
   (rules :accessor rules
          :documentation "hash table, assigns rule instance to name")
   (rete :accessor rete :documentation "the rete singleton instance")
   (activations :initform () :accessor activations
           :documentation "list of matches")
   (undo-stack :initform () :accessor undo-stack
	       :documentation "stacks closures, that restore previous state")
   (redo-stack :initform () :accessor redo-stack
	       :documentation "stacks closures, that restore state before undo")
   (running :initform nil :accessor running)
   (goals :initform () :accessor goals)
   (back-stack :initform () :accessor back-stack
	       :documentation "stack for backtracking during backward chaining
                                 inference"))
  (:documentation "keeps track of defined fact-groups, templates, rules,
                     strategies and watchers and stores the asserted facts
                     and the activations"))

;; PUBLIC INTERFACE
;; constructor:
;(defun make-environment ())
;; undo/redo:
(defgeneric undo (env))
(defgeneric redo (env))
;; watchers:
(defgeneric set-watcher (env watcher &optional undo-label))
(defgeneric unset-watcher (env watcher &optional undo-label))
(defgeneric watched-p (env watcher))
;; templates:
(defgeneric add-template (env template &optional undo-label))
(defgeneric find-template (env name))
(defgeneric print-template (env name))
;; facts:
(defgeneric find-fact (env fact))
(defgeneric add-fact (env fact &optional undo-label))
(defgeneric rem-fact (env fact &optional undo-label))
(defgeneric mod-fact (env old-fact new-fact &optional undo-label))
;; fact groups:
(defgeneric find-fact-group (env group-name))
(defgeneric add-fact-group (env group-name facts &optional undo-label))
(defgeneric rem-fact-group (env group-name &optional undo-label))
;; strategies:
(defgeneric add-strategy (env strat-name function &optional undo-label))
(defgeneric set-strategy (env &optional strat-name undo-label))
;; activations:
;(defgeneric add-match (env production token)) ; forward-declared in rete
;(defgeneric remove-match (env production token)) ; forward-declared in rete
(defgeneric print-activations (env))
;; rules:
(defgeneric add-rule (env rule &optional undo-label))
(defgeneric rem-rule (env rule-name &optional undo-label))
(defgeneric find-rule (env rule-name))
;; environment clean-up:
(defgeneric clear-env (env &optional undo-label))
(defgeneric reset-env (env &optional undo-label))
(defgeneric completely-reset-env (env)) ; DEBUG
;; inference steps
(defgeneric step-env (env &optional undo-label))
(defgeneric halt-env (env))
(defgeneric run-env (env &optional undo-label))
;; backward chaining:
(defgeneric add-goal (env goal &optional undo-label))
(defgeneric print-goals (env))
(defgeneric back-step (env &optional undo-label))
