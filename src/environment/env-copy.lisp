(in-package :exil-env)

; public, used for testing
; TODO: unexport these once properly tested
(defgeneric copy-env (env))
(defgeneric env-copy-p (env1 env2))
(defgeneric common-slots-p (env1 env2))

(defmethod copy-env ((env environment))
  (let ((new-env (make-environment)))
    (with-slots (watchers templates facts fact-groups strategies
			  current-strategy-name rules rete agenda
			  undo-stack redo-stack) new-env
      (setf watchers    (copy-watchers    (watchers env))
	    templates   (copy-templates   (templates env))
	    facts       (copy-list        (facts env))
	    fact-groups (copy-fact-groups (fact-groups env))
	    strategies  (copy-strategies  (strategies env))
	    current-strategy-name         (current-strategy-name env)
	    rules       (copy-rules       (rules env))
	    rete        (copy-rete        (rete env) new-env)
	    agenda      (copy-agenda      (agenda env))
	    undo-stack  (copy-undo-stack  (undo-stack env))
	    redo-stack  (copy-undo-stack  (redo-stack env))))
    new-env))

;; this isn't a general purpose environment equality predicate
;; it's too strict, strategies, undo and redo items are fuctions
;; and as such can only be tested for object equality
;; thus two environments may behave equally, but this predicate
;; will still return nil, if these functions aren't same instances
;; it also uses rete-copy-p, which is too strict too
(defmethod env-copy-p ((env1 environment) (env2 environment))
  (with-slots (watchers templates facts fact-groups strategies
			current-strategy-name rules rete agenda
			undo-stack redo-stack) env1
    (and (equalp         watchers    (watchers env2))
	 (tmpls-equal-p  templates   (templates env2))
	 (facts-equal-p  facts       (facts env2))
	 (fgs-equal-p    fact-groups (fact-groups env2))
	 (strats-equal-p strategies  (strategies env2))
	 (equalp current-strategy-name (current-strategy-name env2))
	 (rules-equal-p  rules       (rules env2))
	 (rete-copy-p    rete        (rete env2))
	 (acts-equal-p   agenda      (agenda env2))
	 (equalp         undo-stack  (undo-stack env2))
	 ;; redo stack mustn't be checked here, because after executing
	 ;; same action and the undoing it, the redo stack now contains
	 ;; the action for possible redo
	 )))

(defmethod common-slots-p (env1 env2)
  (with-slots (watchers templates facts fact-groups strategies
			current-strategy-name rules rete agenda
			undo-stack redo-stack) env1
    (or (eq watchers   (watchers env2))
	(eq templates  (templates env2))
	(eq strategies (strategies env2))
	(eq rules      (rules env2))
	(eq rete       (rete env2))
	(and facts       (eq facts       (facts env2)))
	(and fact-groups (eq fact-groups (fact-groups env2)))
	(and agenda      (eq agenda      (agenda env2)))
	(and undo-stack  (eq undo-stack  (undo-stack env2)))
	(and redo-stack  (eq redo-stack  (redo-stack env2))))))
