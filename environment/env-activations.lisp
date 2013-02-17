(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRATEGIES
;; strategy names are keyword symbols

;; public
(defmethod add-strategy ((env environment) (name symbol) (function function))
  (push-update (cons (to-keyword name) function) (strategies env)))

;; public
(defmethod set-strategy ((env environment) &optional (name :default))
  (if (assoc (to-keyword name) (strategies env))
      (setf (current-strategy-name env) name)
      (error "unknown strategy ~A" name)))

;; private
(defgeneric current-strategy (env))

(defmethod current-strategy ((env environment))
  (assoc-value (current-strategy-name env) (strategies env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTIVATIONS

;; public, used by rete
(defmethod add-match ((env environment) production token)
  (let ((match (make-match production token)))
    ;; when it wasn't already there
    (when (and (nth-value 1 (ext-pushnew match (agenda env)
                                         :test #'match-equal-p))
               (watched-p env :activations))
      (format t "~%==> ~A" match)
      #+lispworks(exil-gui:update-lists))))

;; public, used by rete
(defmethod remove-match ((env environment) production token)
  (let ((match (make-match production token)))
    (multiple-value-bind (new-list altered-p)
        (ext-delete match (agenda env) :test #'match-equal-p)
      (when altered-p
        (setf (agenda env) new-list)
        (when (watched-p env :activations)
          (format t "~%<== ~A" match))
        #+lispworks(exil-gui:update-lists)))))

;; private
(defgeneric remove-matches (env rule)
  (:documentation "remove all matches including given rule"))

(defmethod remove-matches ((env environment) rule)
  (setf (agenda env)
        (delete rule (agenda env)
                :test #'rule-equal-p :key #'match-rule))
  #+lispworks(exil-gui:update-lists))

;; public
(defmethod select-activation ((env environment))
  (let ((activation (funcall (current-strategy env) (agenda env))))
    (setf (agenda env) (delete activation (agenda env)
                               :test #'match-equal-p))
    activation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RULES

;; public
(defmethod add-rule ((env environment) (rule rule))
  (setf (gethash (name rule) (rules env)) rule)
  (new-production (rete env) rule)
  (when (watched-p env :rules)
    (format t "==> ~A" rule))
  (dolist (fact (facts env))
    (add-wme (rete env) fact))
  #+lispworks(exil-gui:update-lists)
  rule)

;; public
(defmethod rem-rule ((env environment) (rule rule))
  (let* ((name (name rule))
         (old-rule (gethash name (rules env))))
    (when old-rule
      (when (watched-p env :rules)
        (format t "<== ~A" old-rule))
      (remhash name (rules env))
      (remove-production (rete env) rule)
      (remove-matches env rule))))

;; public
(defmethod find-rule ((env environment) (name symbol))
  (gethash (to-keyword name) (rules env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT CLEANUP

;; clears facts one by one
;; should be dropped in favor of reset-environment
;; public
(defmethod reset-facts ((env environment))
  (dolist (fact (facts env))
    (rem-fact env fact)))

;; clears facts, agenda and rete, keeps templates and rules
;; if there're are some rules, whose conditions are met by empty set of facts
;; these will appear in the agenda thereafter
;; public
(defmethod reset-environment ((env environment))
  (setf (facts env) ()
        (agenda env) ()
        (rete env) (make-rete env))
  (iter (for (name rule) in-hashtable (rules env))
        (add-rule env rule))
  #+lispworks(exil-gui:update-lists)
  nil)

;; clears everything
;; public, not in use
(defmethod completely-reset-environment ((env environment))
  (setf (facts env) ()
        (agenda env) ()
        (fact-groups env) ()
        (templates env) (make-hash-table :test 'equalp)
        (rules env) (make-hash-table :test 'equalp)
        (rete env) (make-rete env))
  #+lispworks(exil-gui:update-lists)
  nil)
