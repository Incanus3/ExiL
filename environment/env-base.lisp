(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass environment ()
  ((watchers :accessor watchers
             :documentation "alist, (:facts, :rules, :activations) -> t/nil")
   (templates :initform (make-hash-table :test #'equalp) :accessor templates
              :documentation "hash table, assigns template instance to name")
   (facts :initform () :accessor facts
          :documentation "list of fact instances")
   (fact-groups :initform () :accessor fact-groups
                :documentation "((group-name description*)*)")
   (strategies :accessor strategies
               :documentation "alist, assigns strategy function to name symbol")
   (current-strategy-name :initform :depth-strategy
                          :accessor current-strategy-name
                          :documentation "symbol")
   (rules :initform (make-hash-table :test #'equalp) :accessor rules
          :documentation "hash table, assigns rule instance to name")
   (rete :accessor rete :documentation "the rete singleton instance")
   (activations :initform () :accessor activations
           :documentation "list of matches")
   (undo-stack :initform () :accessor undo-stack
	       :documentation "stacks closures, that restore previous state")
   (redo-stack :initform () :accessor redo-stack
	       :documentation "stacks closures, that restore state before undo"))
  (:documentation "keeps track of defined fact-groups, templates, rules,
                     strategies and watchers and stores the asserted facts
                     and the activations"))

;; PUBLIC METHODS
;; constructor:
;(defun make-environment ())
;; undo/redo:
(defgeneric undo (env))
(defgeneric redo (env))
;; watchers:
(defgeneric set-watcher (env watcher &optional undo-label))
(defgeneric unset-watcher (env watcher &optional undo-label))
;; templates:
(defgeneric add-template (env template &optional undo-label))
(defgeneric find-template (env name))
;; facts:
(defgeneric find-fact (env fact))
(defgeneric add-fact (env fact))
(defgeneric rem-fact (env fact))
;; fact groups:
(defgeneric find-fact-group (env group-name))
(defgeneric add-fact-group (env group-name facts &optional undo-label))
(defgeneric rem-fact-group (env group-name &optional undo-label))
;; strategies:
(defgeneric add-strategy (env strat-name function &optional undo-label))
(defgeneric set-strategy (env &optional strat-name undo-label))
;; rules:
(defgeneric add-rule (env rule))
(defgeneric rem-rule (env rule-name))
(defgeneric find-rule (env rule-name))
;; activations:
;(defgeneric add-match (env production token)) ; forward-declared in rete
;(defgeneric remove-match (env production token)) ; forward-declared in rete
(defgeneric select-activation (env))
;; environment clean-up:
(defgeneric clear-env (env))
(defgeneric reset-env (env))
(defgeneric completely-reset-env (env)) ; DEBUG

(defmethod initialize-instance :after ((env environment) &key)
  (with-slots (watchers strategies rete) env
    (setf watchers (copy-alist '((:facts . ()) (:rules . ())
                                 (:activations . ())))
          strategies
          (copy-alist `((:default . ,#'newer-than-p)
			(:depth-strategy . ,#'newer-than-p)
                        (:breadth-strategy . ,#'older-than-p)
                        (:simplicity-strategy . ,#'simpler-than-p)
                        (:complexity-strategy . ,#'more-complex-than-p)))
          rete (make-rete env))))

; public
(defun make-environment ()
  (make-instance 'environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNDO/REDO

(defun stack-for-undo (env undo-fun redo-fun label)
  (push (list undo-fun redo-fun label) (undo-stack env)))

(defun stack-for-redo (env redo-fun undo-fun label)
  (push (list redo-fun undo-fun label) (redo-stack env)))

(defmacro with-undo (env label undo-fun &body body)
  ; redo function has the same body as the original action
  `(progn (stack-for-undo ,env ,undo-fun (lambda () ,@body) ,label)
	  ,@body))

; public
(defmethod undo ((env environment))
  (when (undo-stack env)
    (destructuring-bind (undo-fun redo-fun label) (pop (undo-stack env))
      (funcall undo-fun)
      (stack-for-redo env redo-fun undo-fun label)))
  nil)

; public
(defmethod redo ((env environment))
  (when (redo-stack env)
    (destructuring-bind (redo-fun undo-fun label) (pop (redo-stack env))
      (funcall redo-fun)
      (stack-for-undo env undo-fun redo-fun label)))
  nil)

(defun stack-item-label (item)
  (third item))

(defun numbered-stack (stack)
  (numbered-map #'stack-item-label stack))

(defun print-stack (stack)
  (format t "~:{~5<~a: ~>~a~%~}" (numbered-stack stack)))

; public
(defmethod print-undo-stack ((env environment))
  (print-stack (undo-stack env)))

; public
(defmethod print-redo-stack ((env environment))
  (print-stack (redo-stack env)))
