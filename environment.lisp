(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; public
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass exil-environment ()
    ((facts :initform ())
     (fact-groups :initform ())
     (templates :initform (make-hash-table :test 'equalp))
     (rules :initform (make-hash-table :test 'equalp))
     (rete :initform (make-rete))
     (agenda :initform ())
     (strategies :initform `((default . ,#'depth-strategy)
			     (depth-strategy . ,#'depth-strategy)
			     (breadth-strategy . ,#'breadth-strategy)
			     (simplicity-strategy . ,#'simplicity-strategy)
			     (complexity-strategy . ,#'complexity-strategy)))
     (current-strategy-name :initform 'default)
     (watchers :initform '((:facts . nil)
			   (:rules . nil)
			   (:activations . nil)))))


; not in use
  (defvar *environments*
    (let ((table (make-hash-table :test #'equalp)))
      (setf (gethash "default" table)
	    (make-instance 'exil-environment))
      table))

; not in use
(defmacro defenv (name &key (redefine nil))
  (let ((sym-name (gensym "sym-name")))
    `(let ((,sym-name (symbol-name ,name)))
       (when (or (not (gethash ,name *environments*))
                 ,redefine)
         (setf (gethash ,name *environments*)
               (make-instance 'exil-environment))))))

; not in use
(defmacro setenv (name)
  (let ((env (gensym "env")))
    `(let ((,env (gethash (symbol-name ,name) *environments*)))
       (when ,env (setf *current-environment* ,env)))))

; public
  (defvar *current-environment*
    (gethash "default" *environments*)))

; private
(defmacro exil-env-reader (slot-name)
  `(defmethod ,slot-name ()
     (slot-value *current-environment* ',slot-name)))

; private
(defmacro exil-env-writer (slot-name)
  `(defsetf ,slot-name () (value)
     `(setf (slot-value *current-environment* ',',slot-name) ,value)))

;; creates reader function <slot-name> and writer function set-<slot-name>
;; for the environment class, also creates setf macro


;; i used this instead of easier :accessor possibility, for this way
;; i could supply a default value for the environment parameter
; private
(defmacro exil-env-accessor (slot-name)
  `(progn
     (exil-env-reader ,slot-name)
     (exil-env-writer ,slot-name)))

; private
(defmacro exil-env-accessors (&rest slot-names)
  `(progn ,@(loop for slot-name in slot-names
	       collect `(exil-env-accessor ,slot-name))))

; always put exil-env-* calls on one line, for the automated package creator to work
; public
(eval-when (:compile-toplevel :load-toplevel :execute)
  (exil-env-accessors facts fact-groups templates rules rete agenda strategies current-strategy-name watchers))
;; rete should be removed after proper DEBUG

; private
(defmethod watched-p (watcher)
  (assoc-value (to-keyword watcher) (watchers)))

; public
(defun add-fact (fact)
  (when (nth-value 1 (pushnew-end fact (facts) :test #'fact-equal-p))
    (when (watched-p 'facts)
      (format t "==> ~A~%" fact))
    (add-wme fact)))

; public
(defun rem-fact (fact)
  (multiple-value-bind (new-list altered-p)
      (ext-delete fact (facts) :test #'fact-equal-p)
    (when altered-p
      (setf (facts) new-list)
      (when (watched-p 'facts)
	(format t "<== ~A~%" fact))
      (rem-wme fact))))

(defun modify-fact (fact mod-list)
  (let ((new-fact (copy-fact fact)))
    (doplist (slot-name val mod-list)
      (setf (tmpl-fact-slot-value new-fact slot-name) val))
    (rem-fact fact)
    (add-fact new-fact)))

; public
(defun add-fact-group (group-name fact-descriptions)
  (if (assoc group-name (fact-groups))
      (setf (assoc-value group-name (fact-groups))
	    fact-descriptions)
      (push (cons group-name fact-descriptions)
	    (fact-groups)))
  nil)

(defun rem-fact-group (name)
  (setf (fact-groups) (delete name (fact-groups) :key #'car)))

; public
(defun add-template (template)
  (setf (gethash (symbol-name (name template)) (templates)) template)
  template)

; public
(defmethod find-template (name)
  (gethash (symbol-name name) (templates)))

; public
(defun add-rule (rule)
  (setf (gethash (symbol-name (name rule)) (rules)) rule)
  (new-production rule (rete))
  (when (watched-p 'rules)
    (format t "==> ~A" rule))
  (dolist (fact (facts))
    (add-wme fact))
  rule)

; private
(defmethod remove-matches (rule)
  (setf (agenda)
	(delete rule (agenda)
		:test #'rule-equal-p :key #'match-rule)))

;; ODSTRANIT Z AGENDY VSECHNY MATCHE TYKAJICI SE RULE
; public
(defun rem-rule (rule)
  (let* ((name (symbol-name (name rule)))
	 (old-rule (gethash name (rules))))
    (remhash (symbol-name (name rule)) (rules))
    (when (and old-rule (watched-p 'rules))
      (format t "<== ~A" old-rule))
    (remove-production rule (rete))
    (remove-matches rule)))

; public
(defun find-fact (fact)
  (find-if (lambda (fct) (fact-equal-p fact fct)) (facts)))

; public
(defun find-rule (name)
  (gethash (symbol-name name) (rules)))

; public, used by rete
(defmethod add-match (production token)
  (let ((match (make-match production token)))
    (when (and (nth-value 1 (ext-pushnew match (agenda)
					 :test #'match-equal-p))
	       (watched-p 'activations))
      (format t "==> ~A~%" match))))

; public, used by rete
(defmethod remove-match (production token)
  (let ((match (make-match production token)))
  (multiple-value-bind (new-list altered-p)
      (ext-delete match (agenda) :test #'match-equal-p)
    (when altered-p
      (setf (agenda) new-list)
      (when (watched-p 'activations)
	(format t "<== ~A~%" match))))))

; public
(defmethod add-strategy (name function)
  (if (typep function 'function)
      (push-update (cons name function) (strategies))
      (warn "~A is not a function" function)))

; public
(defmethod set-strategy (&optional (name 'default))
  (if (find name (strategies) :key #'car)
    (setf (current-strategy-name) name)
    (warn "unknown strategy ~A" name)))

; private
(defmethod current-strategy ()
  (assoc-value (current-strategy-name) (strategies)))

; public
(defmethod select-activation ()
  (let ((activation (funcall (current-strategy) (agenda))))
    (setf (agenda) (delete activation (agenda)
			   :test #'match-equal-p))
    activation))

(defmethod is-watcher ((watcher symbol))
  (find (to-keyword watcher) (watchers) :key #'car))

; public
(defmethod set-watcher (watcher)
  (cl:assert (is-watcher watcher)
	     () "I don't know how to watch ~A" watcher)
  (setf (assoc-value (to-keyword watcher) (watchers)) t))

; public
(defmethod unset-watcher (watcher)
  (cl:assert (is-watcher watcher)
	     () "I don't know how to watch ~A" watcher)
  (setf (assoc-value (to-keyword watcher) (watchers)) nil))

(defmethod watch-all ()
  (setf (watchers) (mapcar (lambda (pair) (cons (car pair) t)) (watchers))))

(defmethod unwatch-all ()
  (setf (watchers) (mapcar (lambda (pair) (cons (car pair) nil)) (watchers))))

; public
(defun reset-environment ()
  (setf (facts) ()
	(agenda) ()
	(rete) (make-rete))
  (loop for rule being the hash-values in (rules)
     do (add-rule rule))
  nil)

; public
(defun reset-facts ()
  (dolist (fact (facts))
    (rem-fact fact)))

; public, not in use
(defun completely-reset-environment ()
  (setf (facts) ()
	(agenda) ()
	(fact-groups) ()
	(templates) (make-hash-table :test 'equalp)
	(rules) (make-hash-table :test 'equalp)
	(rete) (make-rete))
  nil)
