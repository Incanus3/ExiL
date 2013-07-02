(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRATEGIES
;; strategy names are keyword symbols

(defun find-strategy (env name)
  (assoc-value name (strategies env)))

(defun add-strategy% (env name function)
  (add-assoc-value name (strategies env) function))

; public
(defmethod add-strategy ((env environment) (name symbol) (function function)
			 &optional (undo-label "(defstrategy)"))
  (let ((key (to-keyword name)))
    (with-undo env undo-label
	(let ((original-function (find-strategy env key)))
	  (lambda () (add-strategy% env key original-function)))
      (add-strategy% env key function))))

(defun set-strategy-name% (env name)
  (setf (current-strategy-name env) name))

(defun set-strategy-name (env name undo-label)
  (with-undo env undo-label
      (let ((original-strategy (current-strategy-name env)))
	(lambda () (set-strategy-name% env original-strategy)))
    (set-strategy-name% env name)))

; public
(defmethod set-strategy ((env environment) &optional (name :default)
					     (undo-label "(setstrategy)"))
  (let ((key (to-keyword name)))
    (if (assoc key (strategies env))
        (set-strategy-name env key undo-label)
        (error "unknown strategy ~A" name))))

(defun current-strategy (env)
  (find-strategy env (current-strategy-name env)))

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
  (set-volatile-slots env () () () () (make-rete env))
  (iter (for (name rule) :in-hashtable (rules env))
        (new-production (rete env) rule)))

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
    (activate-fact-groups env)
    #+lispworks(exil-gui:update-lists)))

;; clears everything
; public, used for testing
(defmethod completely-reset-env ((env environment))
  (setf (facts env) ()
        (activations env) ()
        (fact-groups env) ()
	(undo-stack env) ()
	(redo-stack env) ()
        (templates env) (make-hash-table :test 'equalp)
        (rules env) (make-hash-table :test 'equalp)
        (rete env) (make-rete env))
  #+lispworks(exil-gui:update-lists)
  nil)
