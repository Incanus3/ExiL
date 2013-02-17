(in-package :exil-env)
(declaim (optimize (speed 1) (debug 3) (safety 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; public
(defclass environment ()
  ((facts :initform () :accessor facts
          :documentation "list of fact instances")
   (fact-groups :initform () :accessor fact-groups
                :documentation "((group-name description*)*)")
   (templates :initform (make-hash-table :test 'equalp) :accessor templates
              :documentation "hash table, assigns template instance to name")
   (rules :initform (make-hash-table :test 'equalp) :accessor rules
          :documentation "hash table, assigns rule instance to name")
   (rete :accessor rete :documentation "the rete singleton instance")
   (agenda :initform () :accessor agenda
           :documentation "list of matches")
   (strategies :initform `((default . ,#'depth-strategy)
                           (depth-strategy . ,#'depth-strategy)
                           (breadth-strategy . ,#'breadth-strategy)
                           (simplicity-strategy . ,#'simplicity-strategy)
                           (complexity-strategy . ,#'complexity-strategy))
               :accessor strategies
               :documentation "alist, assigns strategy function to name symbol")
   (current-strategy-name :initform 'default :accessor current-strategy-name
                          :documentation "symbol")
   (watchers :initform '((:facts . nil)
                         (:rules . nil)
                         (:activations . nil))
             :accessor watchers
             :documentation "alist, (:facts, :rules, :activations) -> t/nil"))
  (:documentation "keeps track of defined fact-groups, templates, rules,
                     strategies and watchers and stores the asserted facts
                     and the agenda"))

(defmethod initialize-instance :after ((env environment) &key)
  (setf (rete env) (make-rete env)))

;; public methods
(defgeneric add-fact (env fact))
(defgeneric rem-fact (env fact))
(defgeneric modify-fact (env fact mod-list))
(defgeneric add-fact-group (env group-name descriptions))
(defgeneric rem-fact-group (env group-name))
(defgeneric add-template (env template))
(defgeneric find-template (env name))
(defgeneric add-rule (env rule))
(defgeneric rem-rule (env rule))
(defgeneric find-fact (env fact))
(defgeneric find-rule (env rule-name))
;; forward-declared in rete
;(defgeneric add-match (env production token))
;(defgeneric remove-match (env production token))
(defgeneric add-strategy (env strat-name function))
(defgeneric set-strategy (env &optional strat-name))
(defgeneric select-activation (env))
(defgeneric set-watcher (env watcher))
(defgeneric unset-watcher (env watcher))
(defgeneric watch-all (env))
(defgeneric unwatch-all (env))
(defgeneric reset-environment (env))
(defgeneric reset-facts (env))
;; DEBUG
(defgeneric completely-reset-environment (env))

;; private methods
(defgeneric watched-p (env watcher))
(defgeneric remove-matches (env rule))
(defgeneric current-strategy (env))
(defgeneric is-watcher (env watcher))

;; private
(defmethod watched-p ((env environment) watcher)
  (assoc-value (to-keyword watcher) (watchers env)))

;; public
(defmethod add-fact ((env environment) fact)
  (when (nth-value 1 (pushnew-end fact (facts env) :test #'exil-equal-p))
    (when (watched-p env 'facts)
      (format t "~%==> ~A" fact))
    (add-wme (rete env) fact)
    #+lispworks(exil-gui:update-lists)))

;; public
(defmethod rem-fact ((env environment) fact)
  (multiple-value-bind (new-list altered-p)
      (ext-delete fact (facts env) :test #'exil-equal-p)
    (when altered-p
      (setf (facts env) new-list)
      (when (watched-p env 'facts)
        (format t "~%<== ~A" fact))
      (rem-wme (rete env) fact)
      #+lispworks(exil-gui:update-lists))))

;; modify-fact works for template-facts ONLY!
;; mod-list is a plist mapping slot-name to new value
(defmethod modify-fact ((env environment) (fact fact) (mod-list list))
  (assert (find fact (facts env) :test #'exil-equal-p) ()
          "modify: fact ~A not found in (facts)" fact)
  (let ((new-fact (copy-object fact)))
    (doplist (slot-name val mod-list)
      (setf (object-slot new-fact slot-name) val))
    (rem-fact env fact)
    (add-fact env new-fact)))

;; public
(defmethod add-fact-group ((env environment) group-name descriptions)
  (if (assoc group-name (fact-groups env))
      (setf (assoc-value group-name (fact-groups env))
            descriptions)
      (push (cons group-name descriptions)
            (fact-groups env)))
  nil)

(defmethod rem-fact-group ((env environment) name)
  (setf (fact-groups env) (delete name (fact-groups env) :key #'car)))

;; public
(defmethod add-template ((env environment) template)
  (setf (gethash (symbol-name (name template)) (templates env)) template)
  #+lispworks(exil-gui:update-lists)
  template)

;; public
(defmethod find-template ((env environment) name)
  (gethash (symbol-name name) (templates env)))

;; public
(defmethod add-rule ((env environment) rule)
  (setf (gethash (symbol-name (name rule)) (rules env)) rule)
  (new-production (rete env) rule)
  (when (watched-p env 'rules)
    (format t "==> ~A" rule))
  (dolist (fact (facts env))
    (add-wme (rete env) fact))
  #+lispworks(exil-gui:update-lists)
  rule)

;; private
(defmethod remove-matches ((env environment) rule)
  (setf (agenda env)
        (delete rule (agenda env)
                :test #'rule-equal-p :key #'match-rule))
  #+lispworks(exil-gui:update-lists))

;; public
(defmethod rem-rule ((env environment) rule)
  (let* ((name (symbol-name (name rule)))
         (old-rule (gethash name (rules env))))
    (remhash (symbol-name (name rule)) (rules env))
    (when (and old-rule (watched-p env 'rules))
      (format t "<== ~A" old-rule))
    (remove-production rule (rete env))
    (remove-matches env rule)))

;; public
(defmethod find-fact ((env environment) fact)
  (find fact (facts env) :test #'exil-equal-p))

;; public
(defmethod find-rule ((env environment) name)
  (gethash (symbol-name name) (rules env)))

;; public, used by rete
(defmethod add-match ((env environment) production token)
  (let ((match (make-match production token)))
    (when (and (nth-value 1 (ext-pushnew match (agenda env)
                                         :test #'match-equal-p))
               (watched-p env 'activations))
      (format t "~%==> ~A" match)
      #+lispworks(exil-gui:update-lists))))

;; public, used by rete
(defmethod remove-match ((env environment) production token)
  (let ((match (make-match production token)))
    (multiple-value-bind (new-list altered-p)
        (ext-delete match (agenda env) :test #'match-equal-p)
      (when altered-p
        (setf (agenda env) new-list)
        (when (watched-p env 'activations)
          (format t "~%<== ~A" match))
        #+lispworks(exil-gui:update-lists)))))

;; public
(defmethod add-strategy ((env environment) name function)
  (if (typep function 'function)
      (push-update (cons name function) (strategies env))
      (warn "~A is not a function" function)))

;; public
(defmethod set-strategy ((env environment) &optional (name 'default))
  (if (find name (strategies env) :key #'car)
      (setf (current-strategy-name env) name)
      (warn "unknown strategy ~A" name)))

;; private
(defmethod current-strategy ((env environment))
  (assoc-value (current-strategy-name env) (strategies env)))

;; public
(defmethod select-activation ((env environment))
  (let ((activation (funcall (current-strategy env) (agenda env))))
    (setf (agenda env) (delete activation (agenda env)
                               :test #'match-equal-p))
    activation))

(defmethod is-watcher ((env environment) (watcher symbol))
  (find (to-keyword watcher) (watchers env) :key #'car))

;; public
(defmethod set-watcher ((env environment) watcher)
  (cl:assert (is-watcher env watcher)
             () "I don't know how to watch ~A" watcher)
  (setf (assoc-value (to-keyword watcher) (watchers env)) t))

;; public
(defmethod unset-watcher ((env environment) watcher)
  (cl:assert (is-watcher env watcher)
             () "I don't know how to watch ~A" watcher)
  (setf (assoc-value (to-keyword watcher) (watchers env)) nil))

(defmethod watch-all ((env environment))
  (setf (watchers env) (mapcar (lambda (pair) (cons (car pair) t))
                               (watchers env))))

(defmethod unwatch-all ((env environment))
  (setf (watchers env) (mapcar (lambda (pair) (cons (car pair) nil))
                               (watchers env))))

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
