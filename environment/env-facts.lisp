(in-package :exil-env)

;; according rate of change of environment slots I distinguish between:
;; 1) durable slots - templates, fact-groups and rules
;;      usually set in the beginning and not changed thereafter
;; 2) volatile slots - facts, activations and rete
;;      changed very ofter during inference
;; 3) undo/redo stacks - these are actually pretty volatile, but they need to
;;      be handled separately
;; 4) special slots - watchers, strategies
;;      watchers don't impact the inference and strategies (if the user even
;;      changes them) are usualy set once and we don't want to change them at
;;      all, i.e. not even during complete environment reset

(defun copy-vol-slots (env)
  "returns copy of facts, activations and rete in a list"
  (list (copy-list (facts env))
	(copy-acts (activations env))
	(copy-rete (rete env))))

(defun set-dur-slots (env tmpls fgs rules)
  (setf (templates env) tmpls
	(fact-groups env) fgs
	(rules env) rules))

(defun set-vol-slots (env facts acts rete)
  (setf (facts env) facts
        (activations env) acts
	(rete env) rete))

(defun set-stacks (env ustack rstack)
  (setf (undo-stack env) ustack
	(redo-stack env) rstack))

(defmacro with-saved-vol-slots (env undo-label &body body)
  (let ((env-sym (gensym "env")))
    `(let ((,env-sym ,env))
       (with-undo ,env-sym ,undo-label
	   (let ((orig-vol-slots (copy-vol-slots ,env-sym)))
	     (lambda (env) (apply #'set-vol-slots env orig-vol-slots)))
	 ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEMPLATES

(defun all-facts (env)
  "returns all facts in facts and fact-groups of env"
  (append (facts env) (all-fg-facts env)))

(defun find-fact-with-template (env template)
  "finds first fact in all-facts, that is based on template"
  (find-if (lambda (fact) (equalp (name template) (template-name fact)))
	   (all-facts env)))

(defun add-template% (env template undo-label)
  (with-undo env undo-label
      (let ((original-template (find-template env template)))
	(lambda (env) (set-template env (name template) original-template)))
    (set-template env (name template) template)))

(defun template-used-p (env template)
  (and (find-template env template)
       (find-fact-with-template env template)))

; public
(defmethod add-template ((env environment) (template template)
			 &optional (undo-label "(add-template)"))
  (when (template-used-p env template)
    (error "can't redefine template ~A, because there are existing facts using it"
	   (name template)))
  (add-template% env template undo-label)
  #+lispworks(exil-gui:update-lists)
  template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACTS

(defun add-fact%% (env fact)
  "add fact to facts, print watcher output, notify rete, update gui"
  (when (add-fact% env fact)
    (when (watched-p env :facts)
      (format t "~%==> ~A" fact))
    (add-wme (rete env) fact)
    #+lispworks(exil-gui:update-lists)))

; public
(defmethod add-fact ((env environment) (fact fact) &optional
						     (undo-label "(add-fact)"))
  (with-saved-vol-slots env undo-label
    (add-fact%% env fact)))

;; remove fact from facts, print watcher output, notify rete
; public
(defmethod rem-fact ((env environment) (fact fact) &optional
						     (undo-label "(rem-fact)"))
  (del-fact (new-list altered-p) env fact
    (when altered-p
      (setf (facts env) new-list)
      (when (watched-p env :facts)
        (format t "~%<== ~A" fact))
      (rem-wme (rete env) fact)
      #+lispworks(exil-gui:update-lists))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACT GROUPS

; public
(defmethod add-fact-group ((env environment) (group-name symbol)
                           (facts list) &optional (undo-label "(add-fact-group)"))
  (with-undo env undo-label
      (let ((original-fg (find-fact-group env group-name)))
	(lambda (env) (add-fact-group% env group-name original-fg)))
    (add-fact-group% env group-name facts)))

; public
(defmethod rem-fact-group ((env environment) (group-name symbol)
			   &optional (undo-label "(rem-fact-group)"))
  (with-undo env undo-label
      (let ((original-fg (find-fact-group env group-name)))
	(lambda (env) (add-fact-group% env group-name original-fg)))
    (rem-fact-group% env group-name)))

(defun activate-fact-group (env fact-group)
  (mapc (lambda (fact) (add-fact env fact))
        (fg-facts fact-group)))

(defun activate-fact-groups (env)
  (mapc (lambda (fact-group) (activate-fact-group env fact-group))
        (fact-groups env)))
