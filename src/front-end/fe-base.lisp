(in-package :exil)

;;;; EXPORTED FUNCTIONS AND MACROS:
;;; multiple environments:
;; (defmacro defenv (name &key redefine))
;; (defmacro setenv (name))
;; (defun current-env-name ())
;;; watchers:
;; (defmacro watch (watcher))
;; (defmacro unwatch (watcher))
;;; templates:
;; (defmacro deftemplate (name &body slots))
;; (defmacro ppdeftemplate (name))
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
;; (defmacro setstrategy (name))
;; (defun current-strategy ())
;;; rules:
;; (defmacro defrule (name &body rule))
;; (defmacro undefrule (name))
;; (defmacro ppdefrule (name))
;;; backward chaining:
;; (defmacro defgoal (goal-spec))
;; (defun goals ())
;; (defun back-step ())
;; (defun back-run ())
;;; execution:
;; (defun reset ())
;; (defun step ())
;; (defun halt ())
;; (defun run ())
;;; environment cleanup:
;; (defun clear ())
;; (defun complete-reset ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for multiple environments

(defvar *environments* (make-hash-table))
(defvar *current-environment*)
(defvar *current-env-name*)

;; private
(defun set-env-name (name)
  (setf *current-env-name* name))

;; public
(defun current-env-name ()
  *current-env-name*)

;; public
(defmacro defenv (name &key redefine)
  "define new environment with given name, if redefine is true, will redefine
   existing environment with that name, if one exists"
  (let ((env-name (gensym "env-name")))
    `(let ((,env-name (to-keyword ',name)))
       (when (or (not (gethash ,env-name *environments*))
                 ,redefine)
         (setf (gethash ,env-name *environments*)
               (make-environment))))))

;; public
(defmacro setenv (name)
  "set current environment to one previously defined with name"
  (let ((env (gensym "env"))
        (env-name (gensym "env-name")))
    `(let* ((,env-name (to-keyword ',name))
            (,env (gethash ,env-name *environments*)))
       (when ,env
         (setf *current-environment* ,env)
         (set-env-name ,env-name)))))

(defenv :default)
(setenv :default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; watchers

(defun watch% (watcher)
  (set-watcher *current-environment* watcher
	       (format nil "(watch ~A)" watcher)))

; public
(defmacro watch (watcher)
  "Watch selected item (facts, rules, activations)"
  `(watch% ',watcher))

(defun unwatch% (watcher)
  (unset-watcher *current-environment* watcher
		 (format nil "(unwatch ~A)" watcher)))

; public
(defmacro unwatch (watcher)
  "Unwatch selected item"
  `(unwatch% ',watcher))

(defmacro watched-p (watcher)
  `(exil-env:watched-p *current-environment* ',watcher))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; templates

(defun deftemplate% (name slots)
  (add-template *current-environment* (parse-template name slots)
		(format nil "(deftemplate ~A ~S)" name slots)))

; public
(defmacro deftemplate (name &body slots)
  "define new template"
  `(deftemplate% ',name ',slots))

(defmacro ppdeftemplate (name)
  "print template"
  `(print-template *current-environment* ',name))
