(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRATEGIES
; public
(defmethod add-strategy ((env environment) (name symbol) (function function)
			 &optional (undo-label "(add-strategy)"))
  (with-undo env undo-label
      (let ((original-function (find-strategy env name)))
	(lambda (env) (add-strategy% env name original-function)))
    (add-strategy% env name function))
  nil)

(defun set-strategy-name (env name undo-label)
  (with-undo env undo-label
      (let ((original-strategy (current-strategy-name env)))
	(lambda (env) (set-strategy-name% env original-strategy)))
    (set-strategy-name% env name))
  nil)

; public
(defmethod set-strategy ((env environment) &optional (name :default)
					     (undo-label "(set-strategy)"))
    (if (find-strategy env name)
        (set-strategy-name env name undo-label)
        (error "unknown strategy ~A" name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTIVATIONS

; public, used by rete
(defmethod add-match ((env environment) production token)
  (let ((match (make-match production token)))
    (when (and (add-match% env match)
               (watched-p env :activations))
      (format t "~%==> ~A" match)
      #+lispworks(exil-gui:update-lists))))

; public, used by rete
(defmethod remove-match ((env environment) production token)
  (let ((match (make-match production token)))
    (del-match (new-list altered-p) env match
      (when altered-p
        (setf (activations env) new-list)
        (when (watched-p env :activations)
          (format t "~%<== ~A" match))
        #+lispworks(exil-gui:update-lists)))))

(defun rem-matches-with-rule (env rule)
  (setf (activations env)
        (delete rule (activations env)
                :test #'rule-equal-p :key #'match-rule))
  #+lispworks(exil-gui:update-lists))

(defun select-activation (env)
  (let ((activation (first (sort (activations env) (current-strategy env)))))
    (setf (activations env) (delete activation (activations env)
                                    :test #'match-equal-p))
    activation))

; public
(defmethod print-activations ((env environment))
  (fresh-line)
  (princ (activations env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RULES

; public
(defmethod add-rule ((env environment) (rule rule))
  (add-rule% env rule)
  (new-production (rete env) rule)
  (when (watched-p env :rules)
    (format t "==> ~A" rule))
  (dolist (fact (facts env))
    (add-wme (rete env) fact))
  #+lispworks(exil-gui:update-lists)
  rule)

; public
(defmethod rem-rule ((env environment) (name symbol))
  (let ((rule (find-rule env name)))
    (when rule
      (when (watched-p env :rules)
        (format t "<== ~A" rule))
      (rem-rule% env name)
      (remove-production (rete env) rule)
      (rem-matches-with-rule env rule))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT CLEANUP

;; i shouldn't need to copy the volatile slots, as clear-env resets them
;; to newly created structures, co they can be no longer modified by the
;; environment
(defun clear-undo-fun (env)
  (let ((orig-vol-slots (list (facts env) (activations env) (rete env)))
	(orig-stacks (list (undo-stack env) (redo-stack env))))
    (lambda (env)
      (apply #'set-vol-slots env orig-vol-slots)
      (apply #'set-stacks    env orig-stacks))))

(defun clear-env% (env)
  (set-vol-slots env () () (rete-initform env))
  (set-stacks env () ())
  (dorules (name rule) env
    (new-production (rete env) rule))
  #+lispworks(exil-gui:update-lists))

;; clears volatile slots, keeps durable slots
;; if there're are some rules, whose conditions are met by empty set of facts
;; these will appear in the activations thereafter
; public
(defmethod clear-env ((env environment) &optional (undo-label "(clear-env)"))
  (with-undo env undo-label
      (clear-undo-fun env)
    (clear-env% env)))

; public
(defmethod reset-env ((env environment) &optional (undo-label "(reset-env)"))
  (with-undo env undo-label
      (clear-undo-fun env)
    (clear-env% env)
    (activate-fact-groups env)))

;; clears everything except undo and redo stacks
; public, used for undo testing
(defgeneric almost-completely-reset-env (env))

(defmethod almost-completely-reset-env ((env environment))
  (set-dur-slots env (tmpls-initform) () (rules-initform))
  (set-vol-slots env () () (rete-initform env))
  #+lispworks(exil-gui:update-lists)
  nil)

;; clears everything
; public, used for testing
(defmethod completely-reset-env ((env environment))
  (almost-completely-reset-env env)
  (set-stacks env () ())
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INFERENCE STEPS

;; during do-step, every env slot may actually change as there may be any
;; front-end call in the selected rule's RHS
;; for now, suppose that only fact-changing calls are used (assert, retract, modify)
;; => store facts, activations, rete

;; must return true if the step was done
(defmethod do-step ((env environment) &optional (undo-label "(do-step)"))
  (when (activations env)
    (with-saved-vol-slots env undo-label
      (activate-rule (select-activation env)))
    t))
