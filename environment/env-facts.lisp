(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEMPLATES

; public
(defmethod find-template ((env environment) (name symbol))
  (gethash (to-keyword name) (templates env)))

(defmethod find-template ((env environment) (template template))
  (find-template env (name template)))

(defun all-facts (env)
  "returns all facts in facts and fact-groups of env"
  (apply #'append (facts env) (mapcar #'rest (fact-groups env))))

(defun find-fact-with-template (env template)
  "finds first fact in all-facts, that is based on template"
  (find-if (lambda (fact) (equalp (name template) (template-name fact)))
	   (all-facts env)))

; public
(defmethod add-template ((env environment) template)
  (when (and (find-template env template)
	     (find-fact-with-template env template))
    (error "can't redefine template ~A, because there are existing facts using it"
	   (name template)))
  (setf (gethash (name template) (templates env)) template)
  #+lispworks(exil-gui:update-lists)
  template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACTS

; public
(defmethod find-fact ((env environment) fact)
  (find fact (facts env) :test #'exil-equal-p))

;; add fact to facts, print watcher output, notify rete
; public
(defmethod add-fact ((env environment) fact)
  ;; when the fact wasn't already there
  (when (nth-value 1 (pushnew-end fact (facts env) :test #'exil-equal-p))
    (when (watched-p env :facts)
      (format t "~%==> ~A" fact))
    (add-wme (rete env) fact)
    #+lispworks(exil-gui:update-lists)))

;; remove fact from facts, print watcher output, notify rete
; public
(defmethod rem-fact ((env environment) fact)
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

; public
(defmethod add-fact-group ((env environment) (group-name symbol)
                           (facts list))
  (add-assoc-value (to-keyword group-name) (fact-groups env) facts)
  nil)

; public
(defmethod rem-fact-group ((env environment) (group-name symbol))
  (setf (fact-groups env) (delete (to-keyword group-name)
                                  (fact-groups env) :key #'car)))

(defun activate-fact-group (env fact-group)
  (mapc (lambda (fact) (add-fact env fact))
        (cdr fact-group)))

(defun activate-fact-groups (env)
  (mapc (lambda (fact-group) (activate-fact-group env fact-group))
        (fact-groups env)))
