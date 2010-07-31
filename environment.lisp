(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass exil-environment ()
  ((facts :initform ())
   (fact-groups :initform ())
   (templates :initform (make-hash-table :test 'equalp))
   (rules :initform (make-hash-table :test 'equalp))
   (rete :initform (make-instance 'rete))
   (agenda :initform ())
   (strategies :initform `((default . ,#'first)))
   (current-strategy-name :initform 'default)
   (watchers :initform '((facts . nil)
			 (rules . nil)
			 (activations . nil)))))

(defvar *environments*
  (let ((table (make-hash-table)))
    (setf (gethash "default" table)
	  (make-instance 'exil-environment))
    table))

(defvar *current-environment*
  (gethash "default" *environments*))

(defmacro defenv (name &key (redefine nil))
  (let ((name (symbol-name name)))
    (when (or (not (gethash name *environments*))
	      redefine)
      (setf (gethash name *environments*)
	    (make-instance 'exil-environment)))))

(defmacro setenv (name)
  (let ((env (gethash (symbol-name name) *environments*)))
    (when env (setf *current-environment* env))))

;; creates reader function <slot-name> and writer function set-<slot-name>
;; for the environment class, also creates setf macro
;; i used this instead of easier :accessor possibility, for this way
;; i could supply a default value for the environment parameter
(defmacro exil-env-accessor (slot-name)
  `(progn
     (defgeneric ,slot-name (&optional environment))
     (defmethod ,slot-name (&optional (environment *current-environment*))
       (slot-value environment ',slot-name))
     (defsetf ,slot-name (&optional (environment *current-environment*)) (value)
       `(setf (slot-value ,environment ',',slot-name) ,value))))

(defmacro exil-env-accessors (&rest slot-names)
  `(progn ,@(loop for slot-name in slot-names
	       collect `(exil-env-accessor ,slot-name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (exil-env-accessors facts fact-groups templates rules rete agenda
		      strategies current-strategy-name watchers))
;; rete should be removed after proper DEBUG

(defun add-fact (fact &optional (environment *current-environment*))
  (when (nth-value 1 (ext-pushnew fact (facts environment) :test #'fact-equal-p))
    (when (watched-p 'facts)
      (format t "==> ~A~%" fact))
    (add-wme fact)))

(defun rem-fact (fact &optional (environment *current-environment*))
  (multiple-value-bind (new-list altered-p)
      (ext-delete fact (facts environment) :test #'fact-equal-p)
    (when altered-p
      (setf (facts environment) new-list)
      (when (watched-p 'facts)
	(format t "<== ~A~%" fact))
      (remove-wme fact))))

(defun add-fact-group (group-name fact-descriptions
		       &optional (environment *current-environment*))
  (if (assoc group-name (fact-groups environment))
      (setf (assoc-value group-name (fact-groups environment))
	    fact-descriptions)
      (push (cons group-name fact-descriptions)
	    (fact-groups environment))))

(defun add-template (template &optional (environment *current-environment*))
  (setf (gethash (symbol-name (name template)) (templates environment)) template)
  template)

(defun find-template (name &optional (environment *current-environment*))
  (gethash (symbol-name name) (templates environment)))

(defun add-rule (rule &optional (environment *current-environment*))
  (setf (gethash (symbol-name (name rule)) (rules environment)) rule)
  (new-production rule (rete environment))
  (dolist (fact (facts environment))
    (add-wme fact))
  rule)

;; ODSTRANIT Z AGENDY VSECHNY MATCHE TYKAJICI SE RULE
(defun rem-rule (rule &optional (environment *current-environment*))
  (remhash (symbol-name (name rule)) (rules environment))
  (remove-production rule (rete environment))
  (remove-matches rule environment))

(defun find-rule (name &optional (environment *current-environment*))
  (gethash (symbol-name name) (rules environment)))

;; match is a pair (production token)
;; (there's more effective way to do this, but it doesn't look good :-))
(defun match-production (match)
  (car match))

(defun match-token (match)
  (cdr match))

(defun match-equal-p (match1 match2)
  (and (rule-equal-p (match-production match1)
		     (match-production match2))
       (token-equal-p (match-token match1)
		      (match-token match2))))

(defun activation->string (pair)
  (format nil "Activation:~%~A~%~A~%" (car pair) (cdr pair)))

(defmethod add-match (match &optional (environment *current-environment*))
    (when (and (nth-value 1 (ext-pushnew match (agenda environment)
					 :test #'match-equal-p))
	       (watched-p 'activations))
      (format t "==> ~A" (activation->string match))))

(defmethod remove-match (match &optional (environment *current-environment*))
  (multiple-value-bind (new-list altered-p)
      (ext-delete match (agenda environment) :test #'match-equal-p)
    (when altered-p
      (setf (agenda environment) new-list)
      (when (watched-p 'activations)
	(format t "<== ~A" (activation->string match))))))

(defmethod remove-matches (rule &optional (environment *current-environment*))
  (setf (agenda environment)
	(delete rule (agenda environment)
		:test #'rule-equal-p :key #'match-production)))

(defmethod defstrategy% (name function &optional (environment *current-environment*))
  (if (typep function 'function)
      (push-update (cons name function) (strategies environment))
      (warn "~A is not a function" function)))

(defmacro defstrategy (name function)
  `(defstrategy% ',name ,function))

(defmethod set-strategy (&optional (name 'default) (environment *current-environment*))
  (if (find name (strategies environment) :key #'car)
    (setf (current-strategy-name environment) name)
    (warn "unknown strategy ~A" name)))

(defmethod current-strategy (&optional (environment *current-environment*))
  (assoc-value (current-strategy-name environment) (strategies environment)))

(defmethod select-activation (&optional (environment *current-environment*))
  (let ((activation (funcall (current-strategy environment) (agenda environment))))
    (setf (agenda environment) (delete activation (agenda environment)
				       :test #'match-equal-p))
    activation))

(defmethod watch% (watcher &optional (environment *current-environment*))
  (cl:assert (find watcher (mapcar #'car (watchers environment)))
	     () "I don't know how to watch ~A" watcher)
  (setf (assoc-value watcher (watchers environment)) t))

(defmacro watch (watcher)
  `(watch% ',watcher))

(defmethod unwatch% (watcher &optional (environment *current-environment*))
  (cl:assert (find watcher (mapcar #'car (watchers environment)))
	     () "I don't know how to watch ~A" watcher)
  (setf (assoc-value watcher (watchers environment)) nil))

(defmacro unwatch (watcher)
  `(unwatch% ',watcher))

(defmethod watched-p (watcher &optional (environment *current-environment*))
  (assoc-value watcher (watchers environment)))

(defun reset-environment (&optional (environment *current-environment*))
  (setf (facts environment) ()
;	(fact-groups environment) ()
;	(templates environment) (make-hash-table :test 'equalp)
;	(rules environment) (make-hash-table :test 'equalp)
	(rete environment) (make-instance 'rete))
  (loop for rule being the hash-values in (rules environment)
     do (add-rule rule environment))
  nil)

