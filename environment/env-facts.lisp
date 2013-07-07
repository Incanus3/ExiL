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
  (with-undo env undo-label
      (let ((original-template (find-template env template)))
	(lambda (env) (set-template env (name template) original-template)))
    (set-template env (name template) template)))

(defun template-used-p (env template)
  (and (find-template env template)
       (find-fact-with-template env template)))

; public
(defmethod add-template ((env environment) (template template)
			 &optional (undo-label "(deftemplate)"))
  (when (template-used-p env template)
    (error "can't redefine template ~A, because there are existing facts using it"
	   (name template)))
  (add-template% env template undo-label)
  #+lispworks(exil-gui:update-lists)
  template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACTS

; public
(defmethod add-fact ((env environment) (fact fact))
  "add fact to facts, print watcher output, notify rete"
  (when (add-fact% env fact)
    (when (watched-p env :facts)
      (format t "~%==> ~A" fact))
    (add-wme (rete env) fact)
    #+lispworks(exil-gui:update-lists)))

;; remove fact from facts, print watcher output, notify rete
; public
(defmethod rem-fact ((env environment) (fact fact))
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
                           (facts list) &optional (undo-label "(deffacts)"))
  (with-undo env undo-label
      (let ((original-fg (find-fact-group env group-name)))
	(lambda (env) (add-fact-group% env group-name original-fg)))
    (add-fact-group% env group-name facts)))

; public
(defmethod rem-fact-group ((env environment) (group-name symbol)
			   &optional (undo-label "(remfacts)"))
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
