(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRATEGIES
; public
(defmethod add-strategy ((env environment) (name symbol) (function function)
			 &optional (undo-label "(add-strategy)"))
  (let ((original-function (find-strategy env name)))
    (unless (equalp function original-function)
      (with-undo env undo-label
	  (lambda (env) (add-strategy% env name original-function))
	(add-strategy% env name function))
      nil)))

(defun set-strategy-name (env name undo-label)
  (let ((original-strategy (current-strategy-name env)))
    (unless (equalp name original-strategy)
      (with-undo env undo-label
	  (lambda (env) (set-strategy-name% env original-strategy))
	(set-strategy-name% env name))
      nil)))

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
  (fresh-princ (activations env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RULES

(defun add-rule%% (env rule)
  (add-rule% env rule)
  (new-production (rete env) rule)
  (when (watched-p env :rules)
    (format t "==> ~A" rule))
  (dolist (fact (facts env))
    (add-wme (rete env) fact))
  #+lispworks(exil-gui:update-lists))

(defun rule-already-there (env rule)
  (let ((orig-rule (find-rule env (name rule))))
    (and orig-rule (rule-equal-p orig-rule rule))))

; public
(defmethod add-rule ((env environment) (rule rule) &optional
						     (undo-label "(add-rule)"))
  (unless (rule-already-there env rule)
    (with-saved-slots env (rules rete activations) undo-label
      (add-rule%% env rule)))
  nil)

(defun rem-rule%% (env name rule)
  (when (watched-p env :rules)
    (format t "<== ~A" rule))
  (rem-rule% env name)
  (remove-production (rete env) rule)
  (rem-matches-with-rule env rule))

; public
(defmethod rem-rule ((env environment) (name symbol) &optional
						     (undo-label "(rem-rule)"))
  (let ((rule (find-rule env name)))
    (when rule
      (with-saved-slots env (rules rete activations) undo-label
	(rem-rule%% env name rule)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT CLEANUP

(defun clear-env% (env)
  (reset-slots env (facts activations rete undo-stack redo-stack
                          goals back-stack))
  (dorules (name rule) env
    (new-production (rete env) rule))
  #+lispworks(exil-gui:update-lists))

;; clears volatile slots, keeps durable slots
;; if there're are some rules, whose conditions are met by empty set of facts
;; these will appear in the activations thereafter
; public
(defmethod clear-env ((env environment) &optional (undo-label "(clear-env)"))
  (with-saved-slots env (facts activations rete undo-stack redo-stack
                               goals back-stack) undo-label
    (clear-env% env)))

; public
(defmethod reset-env ((env environment) &optional (undo-label "(reset-env)"))
  (with-saved-slots env (facts activations rete undo-stack redo-stack
                               goals back-stack) undo-label
    (clear-env% env)
    (activate-fact-groups env))
  nil)

;; clears everything except undo and redo stacks
; public, used for undo testing
(defgeneric almost-completely-reset-env (env))

(defmethod almost-completely-reset-env ((env environment))
  (reset-slots env (templates fact-groups rules facts activations rete))
  #+lispworks(exil-gui:update-lists))

;; clears everything
; public, used for testing
(defmethod completely-reset-env ((env environment))
  (reset-slots env (templates fact-groups rules facts activations rete
			      goals undo-stack redo-stack back-stack))
  #+lispworks(exil-gui:update-lists))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INFERENCE STEPS

;; during step-env and run-env, every env slot may actually change as there may
;; be any front-end call in the selected rule's RHS
;; for now, suppose that only fact-changing calls are used (assert, retract, modify)
;; => store facts, activations, rete

;; must return true if the step was done
(defmethod step-env ((env environment) &optional (undo-label "(step-env)"))
  (when (activations env)
    (with-saved-slots env (facts activations rete) undo-label
      (activate-rule (select-activation env)))
    t))

(defmethod halt-env ((env environment))
  (format t "~%Halting")
  (setf (running env) nil))

(defmethod run-env ((env environment) &optional (undo-label "(run-env)"))
  (with-saved-slots env (facts activations rete) undo-label
    (setf (running env) t)
    (iter (while (and (running env) (step-env env))))))
