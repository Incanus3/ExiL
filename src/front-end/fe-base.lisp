(in-package :exil)

;;;; EXPORTED FUNCTIONS AND MACROS:
;;; multiple environments:
;; (defmacro defenv (name &key redefine))
;; (defun defenvf (name &key redefine))
;; (defmacro undefenv (name))
;; (defun undefenvf (name))
;; (defmacro setenv (name))
;; (defun setenvf (name))
;; (defun environments ())
;; (defun current-environment ())

;;; templates:
;; (defmacro deftemplate (name &body slots))
;; (defun deftemplatef (name slots))
;; (defmacro undeftemplate (name))
;; (defun undeftemplatef (name))
;; (defmacro ppdeftemplate (name))
;; (defun find-templatef (name))          ;; => external template representation
;; (defmacro find-template (name))
;; (defun templates ())                   ;; => list of names

;;; facts:
;; (defun facts (&optional start-index end-index at-most))
;; (defmacro assert (&rest fact-specs))
;; (defun assertf (&rest fact-specs))
;; (defmacro retract (&rest fact-specs))
;; (defun retractf (&rest fact-specs))
;; (defun retract-all ())
;; (defmacro modify (fact-spec &rest mod-list))
;; (defun modifyf (fact-spec mod-list))

;;; fact groups:
;; (defmacro deffacts (name &body fact-specs))
;; (defun deffactsf (name fact-specs))
;; (defmacro undeffacts (name))
;; (defun undeffactsf (name))
;; (defun fact-groups ())
;; (defmacro find-fact-group (name))
;; (defun find-fact-groupf (name))
;; TODO: (defmacro ppdeffacts (name))

;;; rules:
;; (defmacro defrule (name &body rule))
;; (defun defrulef (name body))
;; (defmacro undefrule (name))
;; (defun undefrulef (name))
;; (defun rules ())
;; (defmacro find-rule (name))
;; (defun find-rulef (name))
;; (defmacro ppdefrule (name))
;; (defun ppdefrulef (name))
;; (defun agenda ())

;;; strategies:
;; (defmacro defstrategy (name function))
;; (defun defstrategyf (name function))
;; (defmacro undefstrategy (name))
;; (defun undefstrategyf (name))
;; (defmacro setstrategy (name))
;; (defun setstrategyf (name))
;; (defun current-strategy ())
;; (defun strategies ())
;; (defun find-strategy (name))

;;; watchers:
;; (defmacro watch (watcher))
;; (defun watchf (watcher))
;; (defmacro unwatch (watcher))
;; (defun unwatchf (watcher))
;; (defmacro watchedp (watcher))
;; (defun watchedpf (watcher))

;;; forward chaning inference execution:
;; (defun reset ())
;; (defun step ())
;; (defun halt ())
;; (defun run ())

;;; backward chaining inference execution:
;; (defmacro defgoal (goal-spec))
;; (defun defgoalf (goal-spec))
;; TODO: (defmacro undefgoal (goal-spec))
;; TODO: (defun undefgoalf (goal-spec))
;; (defun goals ())
;; (defun back-step ())
;; (defun back-run ())

;;; environment cleanup:
;; (defun clear ())
;; (defun complete-reset ())

;;; undo/redo:
;; (defun undo ())
;; (defun redo ())
;; (defun undo-stack ())
;; (defun redo-stack ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for multiple environments

;(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *environments* (make-hash-table))
(defvar *current-environment*)
(defvar *current-env-name*)

;; private
(defun set-env-name (name)
  (setf *current-env-name* name))

;; forward declaration
#+lispworks(defgeneric exil-gui:make-gui (env))

;; public
(defun defenvf (name &key redefine)
  (let ((env-name (to-keyword name)))
    (if (or (not (gethash env-name *environments*))
            redefine)
      (let ((environment (make-environment)))
        (setf (gethash env-name *environments*)
              environment)
        #+lispworks(set-gui environment (exil-gui:make-gui environment)))
      (error "environment ~A is already defined" name)))
  nil)

;; public
(defmacro defenv (name &key redefine)
  "define new environment with given name, if redefine is true, will redefine
   existing environment with that name, if one exists"
  `(defenvf ',name :redefine ,redefine))

;; public
(defun undefenvf (name)
  (let ((env-name (to-keyword name)))
    (remhash env-name *environments*)))

;; public
(defmacro undefenv (name)
  `(undefenvf ',name))

;; public
(defun setenvf (name)
  (let* ((env-name (to-keyword name))
         (env (gethash env-name *environments*)))
    (if env
        (progn
          (setf *current-environment* env)
          (set-env-name env-name))
        (error "environment ~A isn't defined" name))))

;; public
(defmacro setenv (name)
  "set current environment to one previously defined with name"
  `(setenvf ',name))

;; public
(defun environments ()
  (hash-keys *environments*))

;; public
(defun current-environment ()
  *current-env-name*)

;; used by gui
(defun getenv (name)
  (if name
      (gethash name *environments*)
    *current-environment*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; watchers

(defun watchf (watcher)
  (set-watcher *current-environment* watcher
	       (format nil "(watch ~A)" watcher)))

; public
(defmacro watch (watcher)
  "Watch selected item (facts, rules, activations)"
  `(watchf ',watcher))

(defun unwatchf (watcher)
  (unset-watcher *current-environment* watcher
		 (format nil "(unwatch ~A)" watcher)))

; public
(defmacro unwatch (watcher)
  "Unwatch selected item"
  `(unwatchf ',watcher))

(defun watchedpf (watcher)
  (exil-env:watched-p *current-environment* watcher))

(defmacro watchedp (watcher)
  `(watchedpf ',watcher))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; templates

(defun deftemplatef (name slots)
  (add-template *current-environment* (parse-template name slots)
		(format nil "(deftemplate ~A ~S)" name slots))
  nil)

; public
(defmacro deftemplate (name &body slots)
  "define new template"
  `(deftemplatef ',name ',slots))

(defun undeftemplatef (name)
  (rem-template *current-environment* name
                (format nil "(undeftemplatef ~A)" name))
  nil)

(defmacro undeftemplate (name)
  `(undeftemplatef ',name))

(defmacro ppdeftemplate (name)
  "print template"
  `(print-template *current-environment* ',name))

(defun find-templatef (name)
  (external (exil-env:find-template *current-environment* name)))

(defmacro find-template (name)
  `(find-templatef ',name))

(defun templates ()
  (template-names *current-environment*))
