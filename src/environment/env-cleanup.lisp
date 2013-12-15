(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT CLEANUP

(defun clear-env% (env)
  (reset-slots env (facts agenda rete undo-stack redo-stack
                          goals back-stack))
  (dorules (name rule) env
    (declare (ignore name))
    (new-production (rete env) rule))
  (notify env))

;; clears volatile slots, keeps durable slots
;; if there're are some rules, whose conditions are met by empty set of facts
;; these will appear in the agenda thereafter
;; public
(defmethod clear-env ((env environment) &optional (undo-label "(clear-env)"))
  (with-saved-slots env (facts agenda rete undo-stack redo-stack
                               goals back-stack) undo-label
    (clear-env% env)))

;; public
(defmethod reset-env ((env environment) &optional (undo-label "(reset-env)"))
  (with-saved-slots env (facts agenda rete undo-stack redo-stack
                               goals back-stack) undo-label
    (clear-env% env)
    (activate-fact-groups env))
  nil)

;; clears everything except undo and redo stacks
;; public, used for undo testing
(defgeneric almost-completely-reset-env (env))

(defmethod almost-completely-reset-env ((env environment))
  (reset-slots env (templates fact-groups rules facts agenda rete))
  (notify env))

;; clears everything
;; public, used for testing
(defmethod completely-reset-env ((env environment))
  (reset-slots env (templates fact-groups rules facts agenda rete
			      goals undo-stack redo-stack back-stack))
  (notify env))
