(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRATEGIES
; public
(defmethod add-strategy ((env environment) (name symbol) (function function)
			 &optional (undo-label "(add-strategy)"))
  (with-undo env undo-label
      (let ((original-function (find-strategy env name)))
	(lambda () (add-strategy% env name original-function)))
    (add-strategy% env name function)))

(defun set-strategy-name (env name undo-label)
  (with-undo env undo-label
      (let ((original-strategy (current-strategy-name env)))
	(lambda () (set-strategy-name% env original-strategy)))
    (set-strategy-name% env name)))

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

; public
(defmethod select-activation ((env environment))
  (let ((activation (first (sort (activations env) (current-strategy env)))))
    (setf (activations env) (delete activation (activations env)
                                    :test #'match-equal-p))
    activation))

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

;; according to what environment slots clear actually clears I divide them to:
;; 1) durable slots - watchers, templates, fact-groups, strategies and rules
;; 2) volatile slots - facts, activations, undo/redo stacks and rete

(defun set-volatile-slots (env facts acts ustack rstack rete)
  (setf (facts env) facts
        (activations env) acts
	(undo-stack env) ustack
	(redo-stack env) rstack
        (rete env) rete))

(defun clear-undo-fun (env)
  (let ((original-volatile-slots
	 (list (facts env) (activations env) (undo-stack env) (redo-stack env)
	       (rete env))))
    (lambda () (apply #'set-volatile-slots env original-volatile-slots))))

(defun clear-env% (env)
  (set-volatile-slots env () () () () (rete-initform env))
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
(defmethod almost-completely-reset-env ((env environment))
  (setf (facts env) ()
        (activations env) ()
        (fact-groups env) ()
        (templates env) (templates-initform)
        (rules env) (rules-initform)
        (rete env) (rete-initform env))
  #+lispworks(exil-gui:update-lists)
  nil)

;; clears everything
; public, used for testing
(defmethod completely-reset-env ((env environment))
  (setf (undo-stack env) ()
	(redo-stack env) ())
  (almost-completely-reset-env env)
  nil)
