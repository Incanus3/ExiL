(in-package :exil-env)

;; template- and generic fact and pattern makers - front-end template-object
;; specification parsing

; private
(defun tmpl-slots-spec-p (slots-spec)
  (every-couple (lambda (slot-name slot-val)
                  (declare (ignore slot-val))
                  (keywordp slot-name))
                slots-spec))

; private
(defun clips-tmpl-slots-spec-p (slots-spec)
  (every (lambda (slot-spec)
           (and (listp slot-spec)
                (= (length slot-spec) 2)
                (symbolp (first slot-spec))))
         slots-spec))

; private
(defun tmpl-object-specification-p (specification)
  "is this a template-object specification?"
  (and (listp specification)
       (find-template (first specification))
       (or (null (rest specification))
           (tmpl-slots-spec-p (rest specification))
           (clips-tmpl-slots-spec-p (rest specification)))))

;; extracts slot value from lispy slots specification
;; e.g. (:object box :location hall)
; private
(defun get-slot-val-nonclips (slot-name slots-spec)
  (getf slots-spec (to-keyword slot-name)))

;; extracts slot value from clips-like slots specification
;; e.g. ((object box) (location hall))
; private
(defun get-slot-val-clips (slot-name slots-spec)
  (cpl-assoc-val slot-name slots-spec))

;; extracts slot value from slots specification (used in assert)
; private
(defun get-slot-val (slot-name slots-spec)
  (cond ((tmpl-slots-spec-p slots-spec)
         (get-slot-val-nonclips slot-name slots-spec))
        ((clips-tmpl-slots-spec-p slots-spec)
         (get-slot-val-clips slot-name slots-spec))
        (t (error "~S is not a valid slots specification" slots-spec))))

; private
(defun tmpl-object-class (object-type)
  (ecase object-type
    (fact 'template-fact)
    (pattern 'template-pattern)))

; private
(defun make-simple-object (object-type object-spec)
  (ecase object-type
    (fact (make-simple-fact object-spec))
    (pattern (make-simple-pattern object-spec))))

;; creates template object from generic template object specification
; private
(defun make-tmpl-object (object-type object-spec)
  (let ((template (find-template (first object-spec)))
        (slots-spec (rest object-spec))
        slots)
    (cl:assert template () "can't find template ~A" (first object-spec))
    (doslots (slot-name default template)
      (push (cons slot-name (or (get-slot-val slot-name slots-spec)
                                default
                                (slot-default object-type)))
            slots))
    (make-instance (tmpl-object-class object-type)
                   :tmpl-name (name template)
                   :slots (nreverse slots))))

;; creates object from generic object specification - doesn't support
;; pattern negation and match-var, implemented in make-pattern
; private
(defun make-object (object-type object-spec)
  (if (tmpl-object-specification-p object-spec)
      (make-tmpl-object object-type object-spec)
      (make-simple-object object-type object-spec)))

(defun make-fact (fact-spec)
  (make-object 'fact fact-spec))

; TODO:
; make-pattern should support the ?fact <- <pattern> notation
; it should also support the ~, | and & notations in variable matching
(defun make-pattern (pattern-spec &key (match-var nil))
  (let* ((negated (equalp (first pattern-spec) '-))
         (spec (if negated (rest pattern-spec) pattern-spec))
         (pattern (make-object 'pattern spec)))
    (setf (negated-p pattern) negated)
    (setf (match-var pattern) match-var)
    pattern))

; private, used by modify-fact
(defmethod copy-fact ((fact fact))
  (make-fact (fact-description fact)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; public
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass exil-environment ()
    ((facts :initform () :documentation "list of fact instances")
     (fact-groups :initform ()
                  :documentation "((group-name fact-description*)*)")
     (templates :initform (make-hash-table :test 'equalp)
                :documentation "hash table, assigns template instance to name")
     (rules :initform (make-hash-table :test 'equalp)
            :documentation "hash table, assigns rule instance to name")
     (rete :initform (make-rete) :documentation "the rete singleton instance")
     (agenda :initform () :documentation "list of matches")
     (strategies :initform `((default . ,#'depth-strategy)
                             (depth-strategy . ,#'depth-strategy)
                             (breadth-strategy . ,#'breadth-strategy)
                             (simplicity-strategy . ,#'simplicity-strategy)
                             (complexity-strategy . ,#'complexity-strategy))
                 :documentation "alist, assigns strategy function to name symbol")
     (current-strategy-name :initform 'default :documentation "symbol")
     (watchers :initform '((:facts . nil)
                           (:rules . nil)
                           (:activations . nil))
               :documentation "alist, (:facts, :rules, :activations) -> t/nil"))
    (:documentation "keeps track of defined fact-groups, templates, rules,
                     strategies and watchers and stores the asserted facts
                     and the agenda"))

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

; public
(eval-when (:compile-toplevel :load-toplevel :execute)
  (exil-env-accessors facts fact-groups templates rules rete agenda strategies
                      current-strategy-name watchers))
;; rete should be removed after proper DEBUG

; private
(defmethod watched-p (watcher)
  (assoc-value (to-keyword watcher) (watchers)))

; public
(defun add-fact (fact)
  (when (nth-value 1 (pushnew-end fact (facts) :test #'fact-equal-p))
    (when (watched-p 'facts)
      (format t "==> ~A~%" fact))
    (add-wme fact)
    #+lispworks(exil-gui:update-lists)))

; public
(defun rem-fact (fact)
  (multiple-value-bind (new-list altered-p)
      (ext-delete fact (facts) :test #'fact-equal-p)
    (when altered-p
      (setf (facts) new-list)
      (when (watched-p 'facts)
        (format t "<== ~A~%" fact))
      (rem-wme fact)
      #+lispworks(exil-gui:update-lists))))

(defun modify-fact (fact mod-list)
  (assert (find fact (facts) :test #'fact-equal-p) ()
          "modify: fact ~A not found in (facts)" fact)
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
  #+lispworks(exil-gui:update-lists)
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
  #+lispworks(exil-gui:update-lists)
  rule)

; private
(defmethod remove-matches (rule)
  (setf (agenda)
        (delete rule (agenda)
                :test #'rule-equal-p :key #'match-rule))
  #+lispworks(exil-gui:update-lists))

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
      (format t "==> ~A~%" match)
      #+lispworks(exil-gui:update-lists))))

; public, used by rete
(defmethod remove-match (production token)
  (let ((match (make-match production token)))
    (multiple-value-bind (new-list altered-p)
        (ext-delete match (agenda) :test #'match-equal-p)
      (when altered-p
        (setf (agenda) new-list)
        (when (watched-p 'activations)
          (format t "<== ~A~%" match))
        #+lispworks(exil-gui:update-lists)))))

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
  #+lispworks(exil-gui:update-lists)
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
  #+lispworks(exil-gui:update-lists)
  nil)
