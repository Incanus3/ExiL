(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TEMPLATES

(defun all-facts (env)
  "returns all facts in facts and fact-groups of env"
  (append (facts env) (all-fg-facts env)))

(defun find-fact-with-template (env name)
  "finds first fact in all-facts, that is based on template"
  (find name (all-facts env) :key #'template-name))

(defun template-used-p (env name)
  (and (find-template env name)
       (find-fact-with-template env name)))

(defun ensure-tmpl-not-used (env name action)
  (when (template-used-p env name)
    (error "can't ~A template ~A, because there are existing facts using it"
	   action name)))

(defun add-template% (env template undo-label)
  (let ((original-template (find-template env template)))
    (unless (exil-equal-p template original-template)
      (with-undo env undo-label
	  (lambda (env) (set-template env (name template) original-template))
	(set-template env (name template) template)))))

(defun del-template% (env name undo-label)
  (let ((template (find-template env name)))
    (with-undo env undo-label
        (lambda (env) (set-template env name template))
      (del-template env name))))

;; public
(defmethod add-template ((env environment) (template template)
			 &optional (undo-label "(add-template)"))
  (ensure-tmpl-not-used env (name template) "redefine")
  (add-template% env template undo-label)
                                        ;  #+lispworks(exil-gui:update-lists)
  )

;; public
(defmethod rem-template ((env environment) (name symbol)
			 &optional (undo-label "(del-template)"))
  (ensure-tmpl-not-used env name "undefine")
  (del-template% env name undo-label)
                                        ;  #+lispworks(exil-gui:update-lists)
  )

(defmethod print-template ((env environment) (name symbol))
  (fresh-princ (find-template env name)))
