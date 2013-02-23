(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRATEGIES
;; strategy names are keyword symbols

; public
(defmethod add-strategy ((env environment) (name symbol) (function function))
  (add-assoc-value (to-keyword name) (strategies env) function))

; public
(defmethod set-strategy ((env environment) &optional (name :default))
  (let ((key (to-keyword name)))
    (if (assoc key (strategies env))
        (setf (current-strategy-name env) key)
        (error "unknown strategy ~A" name))))

(defun current-strategy (env)
  (assoc-value (current-strategy-name env) (strategies env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTIVATIONS

; public, used by rete
(defmethod add-match ((env environment) production token)
  (let ((match (make-match production token)))
    ;; when it wasn't already there
    (when (and (nth-value 1 (ext-pushnew match (activations env)
                                         :test #'match-equal-p))
               (watched-p env :activations))
      (format t "~%==> ~A" match)
      #+lispworks(exil-gui:update-lists))))

; public, used by rete
(defmethod remove-match ((env environment) production token)
  (let ((match (make-match production token)))
    (multiple-value-bind (new-list altered-p)
        (ext-delete match (activations env) :test #'match-equal-p)
      (when altered-p
        (setf (activations env) new-list)
        (when (watched-p env :activations)
          (format t "~%<== ~A" match))
        #+lispworks(exil-gui:update-lists)))))

(defun remove-matches (env rule)
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
  (setf (gethash (name rule) (rules env)) rule)
  (new-production (rete env) rule)
  (when (watched-p env :rules)
    (format t "==> ~A" rule))
  (dolist (fact (facts env))
    (add-wme (rete env) fact))
  #+lispworks(exil-gui:update-lists)
  rule)

; public
(defmethod find-rule ((env environment) (name symbol))
  (gethash (to-keyword name) (rules env)))

; public
(defmethod rem-rule ((env environment) (name symbol))
  (let ((rule (find-rule env name)))
    (when rule
      (when (watched-p env :rules)
        (format t "<== ~A" rule))
      (remhash name (rules env))
      (remove-production (rete env) rule)
      (remove-matches env rule))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT CLEANUP

;;; result of calling reset-facts or clear-env is practically same, but
;;; reset-facts is faster when there are many rules (as clear-env recreates the
;;; rete network), whereas clear-env is faster when there are many facts
;;; in the working memory (as retracting facts one by one involves many
;;; rete-node inactivations)
;;; usually there's much more facts than rules, front-end:retract-all thus
;;; uses clear-env

;; clears facts one by one
;; should be dropped in favor of clear-env
; public
;; (defmethod reset-facts ((env environment))
;;   (dolist (fact (facts env))
;;     (rem-fact env fact)))

;; clears facts, activations and rete, keeps templates, fact groups and rules
;; if there're are some rules, whose conditions are met by empty set of facts
;; these will appear in the activations thereafter
; public
(defmethod clear-env ((env environment))
  (setf (facts env) ()
        (activations env) ()
        (rete env) (make-rete env))
  (iter (for (name rule) in-hashtable (rules env))
        (new-production (rete env) rule)))

; public
(defmethod reset-env ((env environment))
  (clear-env env)
  (activate-fact-groups env)
  #+lispworks(exil-gui:update-lists)
  nil)

;; clears everything
; public, not in use
(defmethod completely-reset-env ((env environment))
  (setf (facts env) ()
        (activations env) ()
        (fact-groups env) ()
        (templates env) (make-hash-table :test 'equalp)
        (rules env) (make-hash-table :test 'equalp)
        (rete env) (make-rete env))
  #+lispworks(exil-gui:update-lists)
  nil)
