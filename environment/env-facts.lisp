(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEMPLATES

; public
(defmethod find-template ((env environment) (name symbol))
  "finds template in env with given name"
  (gethash (to-keyword name) (templates env)))

(defmethod find-template ((env environment) (template template))
  "finds template in env with same name as template"
  (find-template env (name template)))

(defun set-template (env name template)
  (setf (gethash name (templates env)) template))

(defun all-facts (env)
  "returns all facts in facts and fact-groups of env"
  (apply #'append (facts env) (mapcar #'rest (fact-groups env))))

(defun find-fact-with-template (env template)
  "finds first fact in all-facts, that is based on template"
  (find-if (lambda (fact) (equalp (name template) (template-name fact)))
	   (all-facts env)))

(defun add-template% (env template)
  (with-undo env
      (let ((original-template (find-template env template)))
	(lambda () (set-template env (name template) original-template)))
      (lambda () (set-template env (name template) template))
    (set-template env (name template) template)))

; public
(defmethod add-template ((env environment) (template template))
  (when (and (find-template env template)
	     (find-fact-with-template env template))
    (error "can't redefine template ~A, because there are existing facts using it"
	   (name template)))
  (add-template% env template)
  #+lispworks(exil-gui:update-lists)
  template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACTS

; public
(defmethod find-fact ((env environment) (fact fact))
  (find fact (facts env) :test #'exil-equal-p))

;; add fact to facts, print watcher output, notify rete
; public
(defmethod add-fact ((env environment) (fact fact))
  ;; when the fact wasn't already there
  (when (nth-value 1 (pushnew-end fact (facts env) :test #'exil-equal-p))
    (when (watched-p env :facts)
      (format t "~%==> ~A" fact))
    (add-wme (rete env) fact)
    #+lispworks(exil-gui:update-lists)))

;; remove fact from facts, print watcher output, notify rete
; public
(defmethod rem-fact ((env environment) (fact fact))
  (multiple-value-bind (new-list altered-p)
      (ext-delete fact (facts env) :test #'exil-equal-p)
    (when altered-p
      (setf (facts env) new-list)
      (when (watched-p env :facts)
        (format t "~%<== ~A" fact))
      (rem-wme (rete env) fact)
      #+lispworks(exil-gui:update-lists))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACT GROUPS

; public
(defmethod find-fact-group ((env environment) (group-name symbol))
  (assoc-value (to-keyword group-name) (fact-groups env)))

(defun add-fact-group% (env name facts)
  (add-assoc-value name (fact-groups env) facts)
  nil)

; public
(defmethod add-fact-group ((env environment) (group-name symbol)
                           (facts list))
  (let ((key (to-keyword group-name)))
    (with-undo env
	(let ((original-fg (find-fact-group env key)))
	  (lambda () (add-fact-group% env key original-fg)))
	(lambda () (add-fact-group% env key facts))
      (add-fact-group% env key facts))))

(defun rem-fact-group% (env name)
  (setf (fact-groups env) (delete name
                                  (fact-groups env) :key #'car)))

; public
(defmethod rem-fact-group ((env environment) (group-name symbol))
  (let ((key (to-keyword group-name)))
    (with-undo env
	(let ((original-fg (find-fact-group env key)))
	  (lambda () (add-fact-group% env key original-fg)))
	(lambda () (rem-fact-group% env key))
    (rem-fact-group% env key))))

(defun activate-fact-group (env fact-group)
  (mapc (lambda (fact) (add-fact env fact))
        (cdr fact-group)))

(defun activate-fact-groups (env)
  (mapc (lambda (fact-group) (activate-fact-group env fact-group))
        (fact-groups env)))
