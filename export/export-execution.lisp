(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; starting execution

; public
(defun reset ()
  "Clear all facts and add all fact groups"
  (clear)
  (dolist (group (fact-groups *current-environment*))
    (assert-group% group)))

; public
(defun step ()
  "Run inference engine for one turn"
  (when (agenda *current-environment*)
    ;; (format t "~%------------------------------------------------------")
    (activate-rule (select-activation *current-environment*))
    t))

(defvar *exil-running* nil)

; public
(defun halt ()
  "Stop the inference engine"
  (format t "~%Halting")
  (setf *exil-running* nil))

; public
(defun run ()
  "Run the infenece engine"
  (setf *exil-running* t)
  (iter (while (and *exil-running* (step)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environment cleanup

; public
(defun clear ()
  "Delete all facts"
  (reset-environment *current-environment*))

;; DEBUG:
(defun complete-reset ()
  (exil-env::completely-reset-environment *current-environment*))
