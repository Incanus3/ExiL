(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AGENDA

;; public, used by rete
(defmethod add-match ((env environment) production token)
  (let ((match (make-match production token)))
    (when (and (add-match% env match)
               (watched-p env :activations))
      (format t "~%==> ~A" match)
      ;; #+lispworks(exil-gui:update-lists)
      )))

;; public, used by rete
(defmethod remove-match ((env environment) production token)
  (let ((match (make-match production token)))
    (del-match (new-list altered-p) env match
      (when altered-p
        (setf (agenda env) new-list)
        (when (watched-p env :activations)
          (format t "~%<== ~A" match))
        ;; #+lispworks(exil-gui:update-lists)
        ))))

(defun rem-matches-with-rule (env rule)
  (setf (agenda env)
        (delete rule (agenda env)
                :test #'rule-equal-p :key #'match-rule))
  ;; #+lispworks(exil-gui:update-lists)
  )

(defun select-match (env)
  (let ((match (first (sort (agenda env) (current-strategy env)))))
    (setf (agenda env) (delete match (agenda env)
                               :test #'match-equal-p))
    match))

;; public
(defmethod print-agenda ((env environment))
  (fresh-princ (agenda env)))
