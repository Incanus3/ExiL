(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEMPLATES

;; public
(defmethod add-template ((env environment) template)
  (setf (gethash (symbol-name (name template)) (templates env)) template)
  #+lispworks(exil-gui:update-lists)
  template)

;; public
(defmethod find-template ((env environment) name)
  (gethash (symbol-name name) (templates env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FACTS

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

(defmethod find-fact ((env environment) fact)
  (find fact (facts env) :test #'exil-equal-p))

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
