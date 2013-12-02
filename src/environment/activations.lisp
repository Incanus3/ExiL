(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environment's agenda consists of matches (CLIPS calls these activations)
;; folowing code implements:
;; 1) variable bindings resolution between rule's conditions (patterns) and the
;;    facts that satisfy them
;; 2) substitution of variables by their bindings in rule's RHS (activations)
;; 3) evaluation of selected match's rule's RHS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-var-bindings% (fact pattern)
  (multiple-value-bind (valid-match bindings)
      (match-against-pattern fact pattern)
    (if valid-match
	bindings
	;; this should never happen
	(error "~A doesn't match ~A" fact pattern))))

(defun get-variable-bindings (fact-list pattern-list)
  (remove-duplicates (mapcan #'get-var-bindings%
			     fact-list pattern-list)
   :test #'equalp))

;; substitute variables in rule's RHS by their bindings
;; if I only used subst, the resulting expression list would share conses
;; with the original, which may cause errors in future
(defun subst-vars-in-activations (activations-with-vars var-bind-list)
  (let ((activations (copy-tree activations-with-vars)))
    (dolist (binding var-bind-list activations)
      (setf activations (nsubst (cdr binding) (car binding) activations)))))

;; resolve variable bindings, substitue variables in RHS and evaluate the
;; RHS expressions
;; TODO: should check (watched-p :activations) before printing output
;; ACTIVATIONS ARE EVALUATED IN DIFFERENT CONTEXT THAN THEY WERE CREATED
;; IT WOULD BE NICE IF THIS COULD BE ENCAPSULATED IN A CLOSURE, BUT THAN
;; I COULDN'T SUBSTITUTE THE VARIABLES
(defun activate-rule (match)
  (let* ((rule (match-rule match))
         (token (match-token match))
         (bindings (get-variable-bindings
                    (token->list token)
		    (remove-if #'negated-p (conditions rule))))
         (activations (subst-vars-in-activations (activations rule)
						 bindings)))
    (format t "~%Firing ~A" (rule-name match))
    (dolist (activation activations)
      (eval activation))))
