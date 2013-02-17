(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEMPLATES

;; public
(defmethod add-template ((env environment) template)
  (setf (gethash (name template) (templates env)) template)
  #+lispworks(exil-gui:update-lists)
  template)

;; public
(defmethod find-template ((env environment) name)
  (gethash (to-keyword name) (templates env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACTS

;; add fact to facts, print watcher output, notify rete
;; public
(defmethod add-fact ((env environment) fact)
  ;; when the fact wasn't already there
  (when (nth-value 1 (pushnew-end fact (facts env) :test #'exil-equal-p))
    (when (watched-p env :facts)
      (format t "~%==> ~A" fact))
    (add-wme (rete env) fact)
    #+lispworks(exil-gui:update-lists)))

;; remove fact from facts, print watcher output, notify rete
;; public
(defmethod rem-fact ((env environment) fact)
  (multiple-value-bind (new-list altered-p)
      (ext-delete fact (facts env) :test #'exil-equal-p)
    (when altered-p
      (setf (facts env) new-list)
      (when (watched-p env :facts)
        (format t "~%<== ~A" fact))
      (rem-wme (rete env) fact)
      #+lispworks(exil-gui:update-lists))))

;; find fact, perform modifications, notify rete
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACT GROUPS

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
