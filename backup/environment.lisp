(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass exil-environment ()
    ((facts :initform ())
     (fact-groups :initform ())
     (templates :initform (make-hash-table :test 'equalp))
     (rules :initform (make-hash-table :test 'equalp))
     (rete :initform (make-instance 'rete))
     (agenda :initform ())
     (strategies :initform `((default . ,#'depth-strategy)
			     (depth-strategy . ,#'depth-strategy)
			     (breadth-strategy . ,#'breadth-strategy)
			     (simplicity-strategy . ,#'simplicity-strategy)
			     (complexity-strategy . ,#'complexity-strategy)))
     (current-strategy-name :initform 'default)
     (watchers :initform '((facts . nil)
			   (rules . nil)
			   (activations . nil)))))

  (defvar *environments*
    (let ((table (make-hash-table :test #'equalp)))
      (setf (gethash "default" table)
	    (make-instance 'exil-environment))
      table))

  (defvar *current-environment*
    (gethash "default" *environments*)))

(defmacro defenv (name &key (redefine nil))
  (let ((sym-name (gensym "sym-name")))
;    `(eval-when (:compile-toplevel :load-toplevel :execute)
    `(let ((,sym-name (symbol-name ,name)))
       (when (or (not (gethash ,name *environments*))
                 ,redefine)
         (setf (gethash ,name *environments*)
               (make-instance 'exil-environment))))))

(defmacro setenv (name)
  (let ((env (gensym "env")))
;    `(eval-when (:compile-toplevel :load-toplevel :execute)
    `(let ((,env (gethash (symbol-name ,name) *environments*)))
       (when ,env (setf *current-environment* ,env)))))

(defmacro exil-env-reader (slot-name)
;  `(eval-when (:compile-toplevel :load-toplevel :execute)
  `(defmethod ,slot-name ()
     (slot-value *current-environment* ',slot-name)))

(defmacro exil-env-writer (slot-name)
;  `(eval-when (:compile-toplevel :load-toplevel :execute)
  `(defsetf ,slot-name () (value)
     `(setf (slot-value *current-environment* ',',slot-name) ,value)))

;; creates reader function <slot-name> and writer function set-<slot-name>
;; for the environment class, also creates setf macro
;; i used this instead of easier :accessor possibility, for this way
;; i could supply a default value for the environment parameter
(defmacro exil-env-accessor (slot-name)
  `(progn
     (exil-env-reader ,slot-name)
     (exil-env-writer ,slot-name)))

(defmacro exil-env-accessors (&rest slot-names)
  `(progn ,@(loop for slot-name in slot-names
	       collect `(exil-env-accessor ,slot-name))))

;(eval-when (:compile-toplevel :load-toplevel :execute)
  (exil-env-accessors fact-groups templates rules rete agenda
		      strategies current-strategy-name watchers)
  (exil-env-writer facts)
;; rete should be removed after proper DEBUG

;; should support environment specification, but don't know, how to combine
;; &optional and &rest, ask someone smarter
(defmethod facts (&rest fact-nums)
  (let ((facts (slot-value *current-environment* 'facts))
	 ;; workaround to at least ignore the environment argument
	(fact-nums (delete-if-not #'numberp fact-nums)))
    (if fact-nums
	(mapcar (lambda (num) (nth num facts)) fact-nums)
	facts)))

(defun add-fact (fact)
  (when (nth-value 1 (ext-pushnew fact (facts) :test #'fact-equal-p))
    (when (watched-p 'facts)
      (format t "==> ~A~%" fact))
    (add-wme fact)))

(defun rem-fact (fact)
  (multiple-value-bind (new-list altered-p)
      (ext-delete fact (facts) :test #'fact-equal-p)
    (when altered-p
      (setf (facts) new-list)
      (when (watched-p 'facts)
	(format t "<== ~A~%" fact))
      (remove-wme fact))))

(defun add-fact-group (group-name fact-descriptions)
  (if (assoc group-name (fact-groups))
      (setf (assoc-value group-name (fact-groups))
	    fact-descriptions)
      (push (cons group-name fact-descriptions)
	    (fact-groups)))
  nil)

(defun add-template (template)
  (setf (gethash (symbol-name (name template)) (templates)) template)
  template)

(defmethod find-template (name)
  (gethash (symbol-name name) (templates)))

(defun add-rule (rule)
  (setf (gethash (symbol-name (name rule)) (rules)) rule)
  (new-production rule (rete))
  (when (watched-p 'rules)
    (format t "==> ~A" rule))
  (dolist (fact (facts))
    (add-wme fact))
  rule)

;; ODSTRANIT Z AGENDY VSECHNY MATCHE TYKAJICI SE RULE
(defun rem-rule (rule)
  (let* ((name (symbol-name (name rule)))
	 (old-rule (gethash name (rules))))
    (remhash (symbol-name (name rule)) (rules))
    (when (and old-rule (watched-p 'rules))
      (format t "<== ~A" old-rule))
    (remove-production rule (rete))
    (remove-matches rule)))

(defun find-rule (name)
  (gethash (symbol-name name) (rules)))

(defmethod add-match (production token)
  (let ((match (make-match production token)))
    (when (and (nth-value 1 (ext-pushnew match (agenda)
					 :test #'match-equal-p))
	       (watched-p 'activations))
      (format t "==> ~A~%" match))))

(defmethod remove-match (production token)
  (let ((match (make-match production token)))
  (multiple-value-bind (new-list altered-p)
      (ext-delete match (agenda) :test #'match-equal-p)
    (when altered-p
      (setf (agenda) new-list)
      (when (watched-p 'activations)
	(format t "<== ~A~%" match))))))

(defmethod remove-matches (rule)
  (setf (agenda)
	(delete rule (agenda)
		:test #'rule-equal-p :key #'match-rule)))

(defmethod defstrategy% (name function)
  (if (typep function 'function)
      (push-update (cons name function) (strategies))
      (warn "~A is not a function" function)))

(defmethod set-strategy% (&optional (name 'default))
  (if (find name (strategies) :key #'car)
    (setf (current-strategy-name) name)
    (warn "unknown strategy ~A" name)))

(defmethod current-strategy ()
  (assoc-value (current-strategy-name) (strategies)))

(defmethod select-activation ()
  (let ((activation (funcall (current-strategy) (agenda))))
    (setf (agenda) (delete activation (agenda)
			   :test #'match-equal-p))
    activation))

(defmethod watch% (watcher)
  (cl:assert (find watcher (mapcar #'car (watchers)))
	     () "I don't know how to watch ~A" watcher)
  (setf (assoc-value watcher (watchers)) t))

(defmethod unwatch% (watcher)
  (cl:assert (find watcher (mapcar #'car (watchers)))
	     () "I don't know how to watch ~A" watcher)
  (setf (assoc-value watcher (watchers)) nil))

(defmethod watched-p (watcher)
  (assoc-value watcher (watchers)))

(defun reset-environment ()
  (setf (facts) ()
	(agenda) ()
	(rete) (make-instance 'rete))
  (loop for rule being the hash-values in (rules)
     do (add-rule rule))
  nil)

(defun completely-reset-environment ()
  (setf (facts) ()
	(agenda) ()
	(fact-groups) ()
	(templates) (make-hash-table :test 'equalp)
	(rules) (make-hash-table :test 'equalp)
	(rete) (make-instance 'rete))
  nil)
