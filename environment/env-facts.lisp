(in-package :exil-env)

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
  (let ((original-template (find-template env template)))
    (unless (exil-equal-p template original-template)
      (with-undo env undo-label
	  (lambda (env) (set-template env (name template) original-template))
	(set-template env (name template) template)))))

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
  (add-fact% env fact)
  (when (watched-p env :facts)
    (format t "~%==> ~A" fact))
  (add-wme (rete env) fact)
  #+lispworks(exil-gui:update-lists))

; public
(defmethod add-fact ((env environment) (fact fact) &optional
						     (undo-label "(add-fact)"))
  (unless (find-fact env fact)
    (with-saved-slots env (facts activations rete) undo-label
      (add-fact%% env fact))))

(defun rem-fact% (env fact)
  (del-fact env fact)
  (when (watched-p env :facts)
    (format t "~%<== ~A" fact))
  (rem-wme (rete env) fact)
  #+lispworks(exil-gui:update-lists))

;; remove fact from facts, print watcher output, notify rete
; public
(defmethod rem-fact ((env environment) (fact fact) &optional
						     (undo-label "(rem-fact)"))
  (when (find-fact env fact)
    (with-saved-slots env (facts activations rete) undo-label
      (rem-fact% env fact))))

(defmethod mod-fact ((env environment) (old-fact fact) (new-fact fact)
		     &optional (undo-label "(mod-fact)"))
  (with-saved-slots env (facts activations rete) undo-label
    (rem-fact% env old-fact)
    (add-fact%% env new-fact)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACT GROUPS

; public
(defmethod add-fact-group ((env environment) (group-name symbol)
                           (facts list) &optional (undo-label "(add-fact-group)"))
  (let ((original-fg (find-fact-group env group-name)))
    (unless (facts-equal-p facts original-fg)
      (with-undo env undo-label
	  (lambda (env) (add-fact-group% env group-name original-fg))
	(add-fact-group% env group-name facts)))))

; public
(defmethod rem-fact-group ((env environment) (group-name symbol)
			   &optional (undo-label "(rem-fact-group)"))
  (let ((original-fg (find-fact-group env group-name)))
    (when original-fg
      (with-undo env undo-label
	  (lambda (env) (add-fact-group% env group-name original-fg))
	(rem-fact-group% env group-name)))))

(defun activate-fact-group (env fact-group)
  (mapc (lambda (fact) (add-fact env fact))
        (fg-facts fact-group)))

(defun activate-fact-groups (env)
  (mapc (lambda (fact-group) (activate-fact-group env fact-group))
        (fact-groups env)))
