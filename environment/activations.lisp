(in-package :exil-env)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environment's activations are themselves matches
;; folowing code implements:
;; 1) variable bindings resolution between rule's conditions (patterns) and the
;;    facts that satisfy them
;; 2) substitution of variables by their bindings in rule's RHS
;;    (which is, confusingly enough, called activations too; this is because
;;    the rete autors call rule's RHS activations, but clips authors use
;;    the term activations for matches in the agenda)
;; 3) evaluation of selected activation's rule's RHS (i.e. its activations :D)
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
(defun subst-vars-in-activations (activations-with-vars var-bind-list)
  (let ((activations (copy-tree activations-with-vars)))
    (dolist (binding var-bind-list activations)
      (setf activations (nsubst (cdr binding) (car binding) activations)))))

;; resolve variable bindings, substitue variables in RHS and evaluate the
;; RHS expressions
;; TODO: should check (watched-p :activations) before printing output
(defun activate-rule (activation)
  (let* ((rule (match-rule activation))
         (token (match-token activation))
         (bindings (get-variable-bindings
                    (token->list token)
		    (remove-if #'negated-p (conditions rule))))
         (activations (subst-vars-in-activations (activations rule)
						 bindings)))
    (format t "~%Firing ~A" (rule-name activation))
    (dolist (activation activations)
      (eval activation))))
