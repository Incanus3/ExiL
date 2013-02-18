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
; (defun hatl ())
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
               (make-instance 'environment))))))

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
  `(progn (if (weak-equal-p ',watcher 'all)
              (watch-all)
              (set-watcher *current-environment* (to-keyword ',watcher)))
          nil))

; public
(defmacro unwatch (watcher)
  "Unwatch selected item"
  `(progn (if (weak-equal-p ',watcher 'all)
              (unwatch-all *current-environment*)
              (unset-watcher *current-environment* (to-keyword ',watcher)))
          nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; templates

(defun nonclips-slot-spec-p (slot-spec)
  (and (symbolp (first slot-spec))
       (or (null (rest slot-spec))
           (keywordp (second slot-spec)))))

(defun clips-slot-spec-p (slot-spec)
  (and (weak-equal-p (first slot-spec) 'slot))
  (symbolp (second slot-spec))
  (listp (nthcdr 2 slot-spec)))

(defun slot-spec-p (slot-spec)
  (or (nonclips-slot-spec-p slot-spec)
      (clips-slot-spec-p slot-spec)))

(defun clips-slot->slot-des% (slot-spec)
  (destructuring-bind (slot slot-name &optional (modifiers nil)) slot-spec
    (declare (ignore slot))
    `(,slot-name . (:default ,(second modifiers)))))

(defun nonclips-slot->slot-des% (slot-spec)
  (destructuring-bind (slot-name &key (default nil)) slot-spec
    `(,slot-name . (:default ,default))))

(defun slot->slot-designator% (slot-spec)
  (cond
    ((nonclips-slot-spec-p slot-spec) (nonclips-slot->slot-des% slot-spec))
    ((clips-slot-spec-p slot-spec) (clips-slot->slot-des% slot-spec))
    (t (error "~A not a valid template slot specifier~%" slot-spec))))

(defun slots->slot-designators% (slots)
  (loop for slot in (to-list-of-lists slots)
     collect (slot->slot-designator% slot)))

;; creates instance of template class with given name and slot specification
;; and pushes it into *templates*.
;; it is to consider whether lambda list (name slots)
;; or (name &body slots) is better
;; for the former possibility, the call is more similar to defclass
;; for the latter, the call is more like defstruct call
; public
(defmacro deftemplate (name &body slots)
  "define new template"
  (let ((template (gensym "template")))
    `(let ((,template
            (make-template ',name
                           ',(slots->slot-designators% slots))))
       (add-template *current-environment* ,template))))
