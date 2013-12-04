(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RULES

(defun add-rule%% (env rule)
  (add-rule% env rule)
  (new-production (rete env) rule)
  (when (watched-p env :rules)
    (format t "==> ~A" rule))
  (dolist (fact (facts env))
    (add-wme (rete env) fact))
  #+lispworks(exil-gui:update-lists)
  )

(defun rule-already-there (env rule)
  (let ((orig-rule (find-rule env (name rule))))
    (and orig-rule (rule-equal-p orig-rule rule))))

                                        ; public
(defmethod add-rule ((env environment) (rule rule) &optional
						     (undo-label "(add-rule)"))
  (unless (rule-already-there env rule)
    (with-saved-slots env (rules rete agenda) undo-label
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
      (with-saved-slots env (rules rete agenda) undo-label
	(rem-rule%% env name rule)))))
