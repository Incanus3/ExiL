(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLOT ABSTRACTION

;; abstract access to slots that are not simple values or lists, this way if
;; the implementation changes (which it does from time to time) I only have to
;; modify these

;; watchers
(defun copy-watchers (watchers)
  (copy-alist watchers))

(defun watchers-initform ()
  (copy-watchers '((:facts . nil) (:rules . nil) (:activations . nil))))

(defun watched-p (env watcher)
  (assoc-value watcher (watchers env)))

(defun is-watcher (env watcher)
  (assoc watcher (watchers env)))

(defun set-one-watcher% (env watcher val)
  (setf (assoc-value watcher (watchers env)) val))

(defun set-all-watchers% (env val)
  (setf (watchers env) (mapcar (lambda (pair) (cons (car pair) val))
			       (watchers env))))

;; templates
(defun copy-templates (templates)
  (copy-hash-table templates))

(defun tmpls-equal-p (templates1 templates2)
  (hash-equal-p templates1 templates2))

(defun tmpls-initform ()
  (make-hash-table :test #'equalp))

; public
(defmethod find-template ((env environment) (name symbol))
  "finds template in env with given name"
  (gethash (to-keyword name) (templates env)))

(defmethod find-template ((env environment) (template template))
  "finds template in env with same name as template"
  (find-template env (name template)))

(defun set-template (env name template)
  (setf (gethash name (templates env)) template))

;; facts
(defun copy-facts (facts)
  (copy-list facts))

(defun facts-equal-p (facts1 facts2)
  (set-equal-p facts1 facts2 :test #'exil-equal-p))

; public
(defmethod find-fact ((env environment) (fact fact))
  (find fact (facts env) :test #'exil-equal-p))

; returns true, if fact was added = wasn't already there
(defun add-fact% (env fact)
  (nth-value 1 (pushnew-end fact (facts env) :test #'exil-equal-p)))

(defmacro del-fact ((new-list altered-p) env fact &body body)
  "destructuring macro"
  `(multiple-value-bind (,new-list ,altered-p)
       (ext-delete ,fact (facts ,env) :test #'exil-equal-p)
     ,@body))

;; fact groups
(defun copy-fact-groups (fact-groups)
  (copy-alist fact-groups))

(defun fg-equal-p (fg1 fg2)
  (and (equalp (car fg1) (car fg2))
       (set-equal-p (cdr fg1) (cdr fg2) :test #'exil-equal-p)))

(defun fgs-equal-p (fgs1 fgs2)
  (set-equal-p fgs1 fgs2 :test #'fg-equal-p))

; public
(defmethod find-fact-group ((env environment) (group-name symbol))
  (assoc-value (to-keyword group-name) (fact-groups env)))

(defun add-fact-group% (env name facts)
  (add-assoc-value (to-keyword name) (fact-groups env) facts)
  nil)

(defun rem-fact-group% (env name)
  (setf (fact-groups env) (delete (to-keyword name) (fact-groups env) :key #'car)))

(defun fg-facts (fg)
  (rest fg))

(defun all-fg-facts (env)
  (mapcan #'fg-facts (fact-groups env)))

;; strategies
(defun copy-strategies (strategies)
  (copy-alist strategies))

(defun strats-initform ()
  (copy-strategies `((:default . ,#'newer-than-p)
		     (:depth-strategy . ,#'newer-than-p)
		     (:breadth-strategy . ,#'older-than-p)
		     (:simplicity-strategy . ,#'simpler-than-p)
		     (:complexity-strategy . ,#'more-complex-than-p))))

(defun strats-equal-p (strats1 strats2)
  (set-equal-p strats1 strats2 :test #'equal))

(defun find-strategy (env name)
  (assoc-value (to-keyword name) (strategies env)))

(defun current-strategy (env)
  (find-strategy env (current-strategy-name env)))

(defun add-strategy% (env name function)
  (add-assoc-value (to-keyword name) (strategies env) function))

(defun set-strategy-name% (env name)
  (setf (current-strategy-name env) (to-keyword name)))

;; activations
(defun copy-activations (activations)
  (mapcar #'copy-match activations))

(defun acts-equal-p (acts1 acts2)
  (set-equal-p acts1 acts2 :test #'match-equal-p))

; returns true if match was added = wasn't already there
(defun add-match% (env match)
  (nth-value 1 (ext-pushnew match (activations env)
			    :test #'match-equal-p)))

(defmacro del-match ((new-list altered-p) env match &body body)
  "destructuring macro"
  `(multiple-value-bind (,new-list ,altered-p)
       (ext-delete ,match (activations ,env) :test #'match-equal-p)
     ,@body))

;; rules
(defun copy-rules (rules)
  (copy-hash-table rules))

(defun rules-initform ()
  (make-hash-table :test #'equalp))

(defun rules-equal-p (rules1 rules2)
  (hash-equal-p rules1 rules2 :test #'rule-equal-p))

; public
(defmethod find-rule ((env environment) (name symbol))
  (gethash (to-keyword name) (rules env)))

(defun add-rule% (env rule)
  (setf (gethash (name rule) (rules env)) rule))

(defun rem-rule% (env name)
  (remhash name (rules env)))

(defmacro dorules ((name rule) env &body body)
  "iteration destructuring macro"
  `(iter (for (,name ,rule) :in-hashtable (rules ,env))
	 ,@body))

;; rete
(defun rete-initform (env)
  (make-rete env))

;; stacks
(defun copy-stack (stack)
  (copy-tree stack))

(defun stack-for-undo (env undo-fun redo-fun label)
  (push (list undo-fun redo-fun label) (undo-stack env))
  nil)

(defmacro pop-undo ((undo-fun redo-fun label) env &body body)
  "destructuring macro"
  `(destructuring-bind (,undo-fun ,redo-fun ,label) (pop (undo-stack ,env))
     ,@body))

(defmacro pop-redo ((redo-fun undo-fun label) env &body body)
  "destructuring macro"
  `(destructuring-bind (,redo-fun ,undo-fun ,label) (pop (redo-stack ,env))
     ,@body))

(defun stack-for-redo (env redo-fun undo-fun label)
  (push (list redo-fun undo-fun label) (redo-stack env))
  nil)

(defun stack-item-label (item)
  (third item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION

(defmethod initialize-instance :after ((env environment) &key)
  (with-slots (watchers templates strategies rules rete) env
    (setf watchers   (watchers-initform)
	  templates  (tmpls-initform)
          strategies (strats-initform)
	  rules      (rules-initform)
          rete       (rete-initform env))))

; public
(defun make-environment ()
  (make-instance 'environment))
