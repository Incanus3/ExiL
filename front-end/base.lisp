(in-package :exil)

;;; EXPORTED FUNCTIONS AND MACROS:
;; multiple environments:
; (defmacro defenv (name &key redefine))
; (defmacro setenv (name))
;; watchers:
; (defmacro watch (watcher))
; (defmacro unwatch (watcher))
;; templates:
; (defmacro deftemplate (name &body slots))
;; facts:
; (defun facts (&optional start-index end-index at-most))
; (defmacro assert (&rest fact-specs))
; (defmacro retract (&rest fact-specs))
; (defun retract-all ())
; (defmacro modify (fact-spec &rest mod-list))
;; fact groups:
; (defmacro deffacts (name &body descriptions))
; (defmacro undeffacts (name))
;; strategies:
; (defmacro defstrategy (name function))
; (defmacro setstrategy (name))
;; rules:
; (defmacro defrule (name &body rule))
; (defmacro undefrule (name))
; (defmacro ppdefrule (name))
;; execution:
; (defun reset ())
; (defun step ())
; (defun halt ())
; (defun run ())
;; environment cleanup:
; (defun clear ())
; (defun complete-reset ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; support for multiple environments

;; private
(defvar *environments* (make-hash-table))
(defvar *current-environment*)

;; public
(defmacro defenv (name &key redefine)
  "define new environment with given name, if redefine is true, will redefine
   existing environment with that name, if one exists"
  (let ((env-name (gensym "env-name")))
    `(let ((,env-name (to-keyword ,name)))
       (when (or (not (gethash ,name *environments*))
                 ,redefine)
         (setf (gethash ,env-name *environments*)
               (make-environment))))))

;; public
(defmacro setenv (name)
  "set current environment to one previously defined with name"
  (let ((env (gensym "env")))
    `(let ((,env (gethash (to-keyword ,name) *environments*)))
       (when ,env (setf *current-environment* ,env)))))

(defenv :default)
(setenv :default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; watchers

; public
(defmacro watch (watcher)
  "Watch selected item (facts, rules, activations)"
  `(progn (if (equalp (to-keyword ',watcher) :all)
              (watch-all *current-environment*)
              (set-watcher *current-environment* (to-keyword ',watcher)))
          nil))

; public
(defmacro unwatch (watcher)
  "Unwatch selected item"
  `(progn (if (equalp (to-keyword ',watcher) :all)
              (unwatch-all *current-environment*)
              (unset-watcher *current-environment* (to-keyword ',watcher)))
          nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; templates

; public
(defmacro deftemplate (name &body slots)
  "define new template"
  `(add-template *current-environment* (parse-template ',name ',slots)))
