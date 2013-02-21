(in-package :exil-parser)

; private
(defun tmpl-slots-spec-p-nonclips (slots-spec)
  "is this lispy-syntax slots specification?"
  (plistp slots-spec))

; private
(defun tmpl-slots-spec-p-clips (slots-spec)
  "is this clips-syntax slots specification?"
  (every (lambda (slot-spec)
           (and (listp slot-spec)
                (= (length slot-spec) 2)
                (symbolp (first slot-spec))))
         slots-spec))

; private
(defun tmpl-slots-spec-p (slots-spec)
  "is this a valid slots specification?"
  (or (tmpl-slots-spec-p-nonclips slots-spec)
      (tmpl-slots-spec-p-clips slots-spec)))

; private
(defgeneric tmpl-object-spec-p (env specification))

(defmethod tmpl-object-spec-p ((env environment) specification)
  "is this a valid template-object specification?"
  (and (listp specification)
       (find-template env (first specification))
       (tmpl-slots-spec-p (rest specification))))

; private
(defun extract-tmpl-name (specification)
  "extract template neme from template-object specification"
  (first specification))

; private
(defun clips->nonclips-spec (slot-spec)
  "convert clips-syntax slots specification to lispy-syntax"
  (let (nonclips-slot-spec)
    (iter (for (slot-name slot-val) in slot-spec)
          (setf (getf nonclips-slot-spec (to-keyword slot-name))
                slot-val))
    nonclips-slot-spec))

; private
(defun extract-slot-spec (fact-spec)
  "extract slots specification from template-object specification"
  (let ((slot-spec (rest fact-spec)))
    (if (tmpl-slots-spec-p-clips slot-spec)
        (clips->nonclips-spec slot-spec)
        slot-spec)))

(defmethod parse-fact ((env environment) fact-spec)
  (if (tmpl-object-spec-p env fact-spec)
      (make-template-fact (find-template env (extract-tmpl-name fact-spec))
                          (extract-slot-spec fact-spec))
      (make-simple-fact fact-spec)))

; TODO: this is actually task for rete
; make-pattern should support the ?fact <- <pattern> notation
; it should also support the ~, | and & notations in variable matching
; public, used by export:defrule
(defmethod parse-pattern ((env environment) pattern-spec &key (match-var nil))
  (let* ((negated (equalp (first pattern-spec) '-))
         (spec (if negated (rest pattern-spec) pattern-spec)))
    (if (tmpl-object-spec-p env spec)
        (make-template-pattern
         (find-template env (extract-tmpl-name spec))
         (extract-slot-spec spec)
         :match-var match-var :negated negated)
        (make-simple-pattern
         spec :match-var match-var :negated negated))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; private
(defun nonclips-mod-list-p (mod-list)
  (plistp mod-list))

; private
(defun clips-mod-list-p (mod-list)
  (alistp mod-list))

; private
(defun clips->nonclips-mod-list (mod-list)
  (iter (for (slot-name new-val) in mod-list)
        (appending (list (to-keyword slot-name) new-val))))

; private
(defun to-mod-spec-list (mod-list)
  (cond
    ((nonclips-mod-list-p mod-list) mod-list)
    ((clips-mod-list-p mod-list) (clips->nonclips-mod-list mod-list))
    (t (error "~A not a valid modify specifier" mod-list))))

;; find fact, perform modifications, notify rete
;; modify-fact works for template-facts ONLY!
;; mod-list is a plist mapping slot-name to new value
(defmethod modify-fact ((fact template-fact) (mod-list list))
;  (assert (find-fact env fact) () "modify: fact ~A not found in (facts)" fact)
  (assert (typep fact 'template-fact) ()
          "modify-fact: simple facts can't be modified")
  (let ((new-fact (copy-object fact)))
    (doplist (slot-name val (to-mod-spec-list mod-list))
      (setf (object-slot new-fact slot-name) val))
;    (rem-fact env fact)
;    (add-fact env new-fact)
    new-fact))
