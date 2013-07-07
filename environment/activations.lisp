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

(defgeneric activate-rule (activation))

;; get variable bindings for single pattern-fact pair
;; TODO: these functions access pattern's internal data (specifier for
;; simple-pattern, slots for template-pattern), which is not very
;; encapsulation-friendly, make iterator macros for simple- and template-
;; objects and use them here. if they're made consistently, the need for
;; saparate variable-bindings methods should also cease
(defgeneric variable-bindings (pattern fact))

(defmethod variable-bindings ((pattern simple-pattern) (fact simple-fact))
  (let ((var-bindings (iter (for atom :in (pattern pattern))
                            (for i :first 0 :then (1+ i))
                            (when (variable-p atom)
                              (collect (cons atom (object-slot fact i))))))
        (match-var (match-var pattern)))
    (if match-var
        (cons (cons match-var (description fact)) var-bindings)
        var-bindings)))

(defmethod variable-bindings ((pattern template-pattern) (fact template-fact))
  (let ((var-bindings (iter (for (slot-name . slot-val) :in (slots pattern))
                            (when (variable-p slot-val)
                              (collect (cons slot-val
                                             (object-slot fact slot-name))))))
        (match-var (match-var pattern)))
    (if match-var
        (cons (cons match-var (description fact)) var-bindings)
        var-bindings)))

;; get variable bindings for corresponding pattern-fact pairs
;; if the variable bindings of particular pattern-fact pairs aren't consistent
;; resulting binding list will include more than one binding for that variable
;; this shouldn't happen if rete functions properly
; private
(defun get-variable-bindings (pattern-list fact-list)
  (remove-duplicates (mapcan #'variable-bindings pattern-list fact-list)
                     :test #'equalp))

;; substitute variables in rule's RHS by their bindings
(defun substitute-variables (activations-with-vars var-bind-list)
  (let ((activations (copy-tree activations-with-vars)))
    (dolist (binding var-bind-list activations)
      (setf activations (subst (cdr binding) (car binding) activations)))))

;; resolve variable bindings, substitue variables in RHS and evaluate the
;; RHS expressions
;; TODO: should check (watched-p :activations) before printing output
; public
(defmethod activate-rule ((activation match))
  (let* ((rule (match-rule activation))
         (token (match-token activation))
         (bindings (get-variable-bindings
                    (remove-if #'negated-p (conditions rule))
                    (token->list token)))
         (activations (substitute-variables (activations rule)
                                            bindings)))
    (format t "~%Firing ~A" (rule-name activation))
    (dolist (activation activations)
      (eval activation))))
