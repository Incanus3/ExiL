(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environment cleanup

; public
(defun clear ()
  "delete all facts"
  (clear-env *current-environment*))

;; DEBUG:
(defun complete-reset ()
  (completely-reset-env *current-environment*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; starting execution

;; TODO: move this behavior to environment
; public
(defun reset ()
  "clear all facts and add all fact groups"
  (reset-env *current-environment* "(reset)"))

; public
(defun step ()
  "run inference engine for one turn"
  (step-env *current-environment* "(step)"))

; public
(defun halt ()
  "stop the inference engine"
  (halt-env *current-environment*))

; public
(defun run ()
  "run the infenece engine"
  (run-env *current-environment* "(run)"))

; public
(defun back-step ()
  (eenv:back-step *current-environment*))

; public
(defun back-run ()
  (eenv:back-run *current-environment*))

; public
(defun undo ()
  "undo the last action"
  (eenv:undo *current-environment*))

; public
(defun redo ()
  "redo the last undone action"
  (eenv:redo *current-environment*))

; public
(defun undo-stack ()
  (print-undo-stack *current-environment*))

; public
(defun redo-stack ()
  (print-redo-stack *current-environment*))
