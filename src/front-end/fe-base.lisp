(in-package :exil)

;;;; EXPORTED FUNCTIONS AND MACROS:
;;; multiple environments:
;; (defmacro defenv (name &key redefine))
;; (defun defenvf (name &key redefine))
;; (defmacro setenv (name))
;; (defun setenvf (name))
;; (defun current-environment ())

;;; watchers:
;; (defmacro watch (watcher))
;; (defun watchf (watcher))
;; (defmacro unwatch (watcher))
;; (defun unwatchf (watcher))
;; (defmacro watchedp (watcher))
;; (defun watchedpf (watcher))

;;; templates:
;; (defmacro deftemplate (name &body slots))
;; (defun deftemplatef (name slots))
;; (defmacro undeftemplate (name))
;; (defun undeftemplatef (name))
;; (defmacro ppdeftemplate (name))
;; (defun find-template (name)) => external template representation
;; (defun templates ())         => list of names

;;; facts:
;; (defun facts (&optional start-index end-index at-most))
;; (defmacro assert (&rest fact-specs))
;; (defmacro retract (&rest fact-specs))
;; (defun retract-all ())
;; (defmacro modify (fact-spec &rest mod-list))

;;; fact groups:
;; (defmacro deffacts (name &body descriptions))
;; (defmacro undeffacts (name))
;; TODO: implement ppdeffacts

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

;;; rules:
;; (defmacro defrule (name &body rule))
;; (defmacro undefrule (name))
;; (defmacro ppdefrule (name))

;;; forward chaning inference execution:
;; (defun reset ())
;; (defun step ())
;; (defun halt ())
;; (defun run ())

;;; backward chaining inference execution:
;; (defmacro defgoal (goal-spec))
;; (defun goals ())
;; (defun back-step ())
;; (defun back-run ())

;;; environment cleanup:
;; (defun clear ())
;; (defun complete-reset ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for multiple environments

(defvar *environments* (make-hash-table))

(defun environments ()
  (hash-keys *environments*))

(defvar *current-environment*)
(defvar *current-env-name*)

;; private
(defun set-env-name (name)
  (setf *current-env-name* name))

;; public
(defun current-environment ()
  *current-env-name*)

(defun defenvf (name &key redefine)
  (let ((env-name (to-keyword name)))
    (when (or (not (gethash env-name *environments*))
              redefine)
       (setf (gethash env-name *environments*)
             (make-environment)))))

;; public
(defmacro defenv (name &key redefine)
  "define new environment with given name, if redefine is true, will redefine
   existing environment with that name, if one exists"
  `(defenvf ',name :redefine ,redefine))

(defun setenvf (name)
  (let* ((env-name (to-keyword name))
          (env (gethash env-name *environments*)))
     (when env
       (setf *current-environment* env)
       (set-env-name env-name))))

;; public
(defmacro setenv (name)
  "set current environment to one previously defined with name"
  `(setenvf ',name))

(defenv :default)
(setenv :default)

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

(defun find-template (name)
  (external (exil-env:find-template *current-environment* name)))

(defun templates ()
  (template-names *current-environment*))
