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
  (reset-env *current-environment*))

; public
(defun step ()
  "run inference engine for one turn"
  (do-step *current-environment*))

; public
(defun halt ()
  "stop the inference engine"
  (halt-env *current-environment*))

; public
(defun run ()
  "run the infenece engine"
  (run-env *current-environment*))

; public
(defun undo ()
  "undo the last action"
  (eenv:undo *current-environment*))

(defun redo ()
  "redo the last undone action"
  (eenv:redo *current-environment*))

; public
(defun undo-stack ()
  (print-undo-stack *current-environment*))

; public
(defun redo-stack ()
  (print-redo-stack *current-environment*))
