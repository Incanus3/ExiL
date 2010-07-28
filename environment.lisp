(in-package :exil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass exil-environment ()
  ((facts :initform ())
   (fact-groups :initform ())
   (templates :initform (make-hash-table :test 'equalp))
   (rules :initform (make-hash-table :test 'equalp))
   (rete :initform (make-instance 'rete))
   (agenda :initform ())
   (strategies :initform ())
   (current-strategy-name :initform 'default)))

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
     (defmethod ,slot-name (&optional (environment *current-environment*))
       (slot-value environment ',slot-name))
     (defsetf ,slot-name (&optional (environment *current-environment*)) (value)
       `(setf (slot-value ,environment ',',slot-name) ,value))))

(defmacro exil-env-accessors (&rest slot-names)
  `(progn ,@(loop for slot-name in slot-names
	       collect `(exil-env-accessor ,slot-name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (exil-env-accessors facts fact-groups templates rules rete agenda
		      strategies current-strategy-name))
;; rete should be removed after proper DEBUG

(defun add-fact (fact &optional (environment *current-environment*))
  (my-pushnew fact (facts environment) :test #'fact-equal-p)
  (add-wme fact))

(defun rem-fact (fact &optional (environment *current-environment*))
  (setf (facts environment)
	(delete fact (facts environment) :test #'fact-equal-p))
  (remove-wme fact))

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

(defmethod add-match (match &optional (environment *current-environment*))
;  (format t "NEW MATCH: ~A~%" match)
  (pushnew match (agenda environment) :test #'match-equal-p))

(defmethod remove-match (match &optional (environment *current-environment*))
;  (format t "BRAKING MATCH: ~A~%" match)
  (setf (agenda environment) 
	(delete match (agenda environment) :test #'match-equal-p)))

(defmethod remove-matches (rule &optional (environment *current-environment*))
  (setf (agenda environment)
	(delete rule (agenda environment)
		:test #'rule-equal-p :key #'match-production)))

(defmethod defstrategy (name function &optional (environment *current-environment*))
  (push-update (cons name function) (strategies environment)))

(defmethod set-strategy (&optional (name 'default) (environment *current-environment*))
  (if (find name (strategies environment) :key #'car)
    (setf (current-strategy-name environment) name)
    (warn "unknown strategy ~A" name)))

(defmethod current-strategy (&optional (environment *current-environment*))
  (assoc-value (current-strategy-name environment) (strategies environment)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstrategy 'default #'first))

(defun reset-environment (&optional (environment *current-environment*))
  (setf (facts environment) ()
;	(fact-groups environment) ()
;	(templates environment) (make-hash-table :test 'equalp)
;	(rules environment) (make-hash-table :test 'equalp)
	(rete environment) (make-instance 'rete))
  (loop for rule being the hash-values in (rules environment)
     do (add-rule rule environment))
  nil)

