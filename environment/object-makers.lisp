(in-package :exil-env)

;; template- and generic fact and pattern makers - front-end template-object
;; specification parsing

;; functions that (potentially) create template objects are defined as methods
;; of environment, as they need access to environment's template list
(defgeneric tmpl-object-spec-p (env specification))
(defgeneric make-tmpl-object (env object-type object-spec))
(defgeneric make-object (env object-type object-spec))
(defgeneric make-fact (env fact-spec))
(defgeneric make-pattern (env pattern-spec &key match-var))

; private, used by tmpl-slots-spec-p, get-slot-val
(defun tmpl-slots-spec-p-nonclips (slots-spec)
  (every-couple (lambda (slot-name slot-val)
                  (declare (ignore slot-val))
                  (keywordp slot-name))
                slots-spec))

; private, used by tmpl-slots-spec-p, get-slot-val
(defun tmpl-slots-spec-p-clips (slots-spec)
  (every (lambda (slot-spec)
           (and (listp slot-spec)
                (= (length slot-spec) 2)
                (symbolp (first slot-spec))))
         slots-spec))

; private, used by tmpl-object-spec-p
(defun tmpl-slots-spec-p (slots-spec)
  (or (tmpl-slots-spec-p-nonclips slots-spec)
      (tmpl-slots-spec-p-clips slots-spec)))

; private, used by make-object
(defmethod tmpl-object-spec-p ((env environment) specification)
  "is this a template-object specification?"
  (and (listp specification)
       (find-template env (first specification))
       (tmpl-slots-spec-p (rest specification))))

(defun extract-tmpl-name (specification)
  (first specification))

(defun clips->nonclips-spec (slot-spec)
  (let (nonclips-slot-spec)
    (iter (for (slot-name slot-val) in slot-spec)
          (setf (getf nonclips-slot-spec (to-keyword slot-name))
                slot-val))
    nonclips-slot-spec))

(defun extract-slot-spec (fact-spec)
  (let ((slot-spec (rest fact-spec)))
    (if (tmpl-slots-spec-p-clips slot-spec)
        (clips->nonclips-spec slot-spec)
        slot-spec)))

(defmethod make-fact ((env environment) fact-spec)
  (if (tmpl-object-spec-p env fact-spec)
      (make-template-fact (find-template env (extract-tmpl-name fact-spec))
                          (extract-slot-spec fact-spec))
      (make-simple-fact fact-spec)))

; TODO:
; make-pattern should support the ?fact <- <pattern> notation
; it should also support the ~, | and & notations in variable matching
; public, used by export:defrule
(defmethod make-pattern ((env environment) pattern-spec &key (match-var nil))
  (let* ((negated (equalp (first pattern-spec) '-))
         (spec (if negated (rest pattern-spec) pattern-spec)))
    (if (tmpl-object-spec-p env spec)
        (make-template-pattern
         (find-template env (extract-tmpl-name spec))
         (extract-slot-spec spec)
         :match-var match-var :negated negated)
        (make-simple-pattern
         spec :match-var match-var :negated negated))))
