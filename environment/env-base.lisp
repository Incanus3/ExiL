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
   (current-strategy-name :initform :default :accessor current-strategy-name
                          :documentation "symbol")
   (rules :initform (make-hash-table :test #'equalp) :accessor rules
          :documentation "hash table, assigns rule instance to name")
   (rete :accessor rete :documentation "the rete singleton instance")
   (agenda :initform () :accessor agenda
           :documentation "list of matches"))
  (:documentation "keeps track of defined fact-groups, templates, rules,
                     strategies and watchers and stores the asserted facts
                     and the agenda"))

;; PUBLIC METHODS
;; watchers:
(defgeneric set-watcher (env watcher))
(defgeneric unset-watcher (env watcher))
(defgeneric watch-all (env))
(defgeneric unwatch-all (env))
;; environment clean-up:
(defgeneric reset-environment (env))
(defgeneric reset-facts (env))
(defgeneric completely-reset-environment (env)) ; DEBUG
;; templates:
(defgeneric add-template (env template))
(defgeneric find-template (env name))
;; facts:
(defgeneric add-fact (env fact))
(defgeneric rem-fact (env fact))
(defgeneric modify-fact (env fact mod-list))
(defgeneric find-fact (env fact))
;; fact groups:
(defgeneric add-fact-group (env group-name descriptions))
(defgeneric rem-fact-group (env group-name))
;; strategies:
(defgeneric add-strategy (env strat-name function))
(defgeneric set-strategy (env &optional strat-name))
;; rules:
(defgeneric add-rule (env rule))
(defgeneric rem-rule (env rule))
(defgeneric find-rule (env rule-name))
;; activations:
;(defgeneric add-match (env production token)) ; forward-declared in rete
;(defgeneric remove-match (env production token)) ; forward-declared in rete
(defgeneric select-activation (env))

(defmethod initialize-instance :after ((env environment) &key)
  (with-slots (watchers strategies rete) env
    (setf watchers (copy-alist '((:facts . ()) (:rules . ())
                                 (:activations . ())))
          strategies
          (copy-alist `((:default . ,#'depth-strategy)
                        (:depth-strategy . ,#'depth-strategy)
                        (:breadth-strategy . ,#'breadth-strategy)
                        (:simplicity-strategy . ,#'simplicity-strategy)
                        (:complexity-strategy . ,#'complexity-strategy)))
          rete (make-rete env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WATCHERS

;; private
(defgeneric watched-p (env watcher))

(defmethod watched-p ((env environment) watcher)
  (assoc-value watcher (watchers env)))

;; private
(defgeneric is-watcher (env watcher))

(defmethod is-watcher ((env environment) (watcher symbol))
  (assoc watcher (watchers env)))

;; public
(defmethod set-watcher ((env environment) watcher)
  (cl:assert (is-watcher env watcher)
             () "I don't know how to watch ~A" watcher)
  (setf (assoc-value watcher (watchers env)) t))

;; public
(defmethod unset-watcher ((env environment) watcher)
  (cl:assert (is-watcher env watcher)
             () "I don't know how to watch ~A" watcher)
  (setf (assoc-value watcher (watchers env)) nil))

(defmethod watch-all ((env environment))
  (setf (watchers env) (mapcar (lambda (pair) (cons (car pair) t))
                               (watchers env))))

(defmethod unwatch-all ((env environment))
  (setf (watchers env) (mapcar (lambda (pair) (cons (car pair) nil))
                               (watchers env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT CLEANUP

;; public
(defmethod reset-environment ((env environment))
  (setf (facts env) ()
        (agenda env) ()
        (rete env) (make-rete env))
  (loop for rule being the hash-values in (rules env)
     do (add-rule env rule))
  #+lispworks(exil-gui:update-lists)
  nil)

;; public
(defmethod reset-facts ((env environment))
  (dolist (fact (facts env))
    (rem-fact env fact)))

;; public, not in use
(defmethod completely-reset-environment ((env environment))
  (setf (facts env) ()
        (agenda env) ()
        (fact-groups env) ()
        (templates env) (make-hash-table :test 'equalp)
        (rules env) (make-hash-table :test 'equalp)
        (rete env) (make-rete env))
  #+lispworks(exil-gui:update-lists)
  nil)
