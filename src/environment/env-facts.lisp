(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACTS

(defun add-fact%% (env fact)
  "add fact to facts, print watcher output, notify rete, update gui"
  (add-fact% env fact)
  (when (watched-p env :facts)
    (fresh-format t "==> ~A" fact))
  (add-wme (rete env) fact)
  #+lispworks(exil-gui:update-lists)
  )

; public
(defmethod add-fact ((env environment) (fact fact) &optional
						     (undo-label "(add-fact)"))
  (unless (find-fact env fact)
    (with-saved-slots env (facts agenda rete) undo-label
      (add-fact%% env fact)))
  nil)

(defun rem-fact% (env fact)
  (del-fact env fact)
  (when (watched-p env :facts)
    (fresh-format t "<== ~A" fact))
  (rem-wme (rete env) fact)
  #+lispworks(exil-gui:update-lists)
  )

;; remove fact from facts, print watcher output, notify rete
; public
(defmethod rem-fact ((env environment) (fact fact) &optional
						     (undo-label "(rem-fact)"))
  (when (find-fact env fact)
    (with-saved-slots env (facts agenda rete) undo-label
      (rem-fact% env fact)))
  nil)

(defmethod mod-fact ((env environment) (old-fact fact) (new-fact fact)
		     &optional (undo-label "(mod-fact)"))
  (unless (and (not (find-fact env old-fact))
	       (find-fact env new-fact))
    (with-saved-slots env (facts agenda rete) undo-label
      (rem-fact% env old-fact)
      (add-fact%% env new-fact)))
  nil)

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
