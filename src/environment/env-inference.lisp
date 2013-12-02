(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INFERENCE STEPS

;; during step-env and run-env, every env slot may actually change as there may
;; be any front-end call in the selected rule's RHS
;; for now, suppose that only fact-changing calls are used
;;   (assert, retract, modify)
;; => store facts, agenda, rete

;; must return true if the step was done
(defmethod step-env ((env environment) &optional (undo-label "(step-env)"))
  (when (agenda env)
    (with-saved-slots env (facts agenda rete) undo-label
      (activate-rule (select-match env)))
    t))

(defmethod halt-env ((env environment))
  (format t "~%Halting")
  (setf (running env) nil))

(defmethod run-env ((env environment) &optional (undo-label "(run-env)"))
  (with-saved-slots env (facts agenda rete) undo-label
    (setf (running env) t)
    (iter (while (and (running env) (step-env env))))))
