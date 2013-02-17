(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRATEGIES

;; public
(defmethod add-strategy ((env environment) name function)
  (if (typep function 'function)
      (push-update (cons name function) (strategies env))
      (warn "~A is not a function" function)))

;; public
(defmethod set-strategy ((env environment) &optional (name 'default))
  (if (find name (strategies env) :key #'car)
      (setf (current-strategy-name env) name)
      (warn "unknown strategy ~A" name)))

;; private
(defgeneric current-strategy (env))

(defmethod current-strategy ((env environment))
  (assoc-value (current-strategy-name env) (strategies env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACTIVATIONS

;; public, used by rete
(defmethod add-match ((env environment) production token)
  (let ((match (make-match production token)))
    (when (and (nth-value 1 (ext-pushnew match (agenda env)
                                         :test #'match-equal-p))
               (watched-p env 'activations))
      (format t "~%==> ~A" match)
      #+lispworks(exil-gui:update-lists))))

;; public, used by rete
(defmethod remove-match ((env environment) production token)
  (let ((match (make-match production token)))
    (multiple-value-bind (new-list altered-p)
        (ext-delete match (agenda env) :test #'match-equal-p)
      (when altered-p
        (setf (agenda env) new-list)
        (when (watched-p env 'activations)
          (format t "~%<== ~A" match))
        #+lispworks(exil-gui:update-lists)))))

;; private
(defgeneric remove-matches (env rule))

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
(defmethod add-rule ((env environment) rule)
  (setf (gethash (symbol-name (name rule)) (rules env)) rule)
  (new-production (rete env) rule)
  (when (watched-p env 'rules)
    (format t "==> ~A" rule))
  (dolist (fact (facts env))
    (add-wme (rete env) fact))
  #+lispworks(exil-gui:update-lists)
  rule)

;; public
(defmethod rem-rule ((env environment) rule)
  (let* ((name (symbol-name (name rule)))
         (old-rule (gethash name (rules env))))
    (remhash (symbol-name (name rule)) (rules env))
    (when (and old-rule (watched-p env 'rules))
      (format t "<== ~A" old-rule))
    (remove-production rule (rete env))
    (remove-matches env rule)))

;; public
(defmethod find-rule ((env environment) name)
  (gethash (symbol-name name) (rules env)))
