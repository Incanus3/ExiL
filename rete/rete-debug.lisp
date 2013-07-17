(in-package :exil-rete)

;; DEBUG:
; uses mop - probably not portable
(defun method-defined-p (name object)
  (compute-applicable-methods (ensure-generic-function name) (list object)))

(defun map-rete% (fun rete)
  (mapcar fun (rete-nodes rete)))

(defmethod map-rete ((funname symbol) (rete rete))
  (map-rete% (lambda (node)
	       (when (method-defined-p funname node)
		 (funcall funname node)))
	     rete))

(defmethod map-rete-if-defined ((function function) (rete rete) (funname symbol))
  (map-rete% (lambda (node)
	       (when (method-defined-p funname node)
		 (funcall function node)))
	     rete))

(defmethod rete-find-nodes-if ((function function) (rete rete) (funname symbol))
  (remove-if-not (lambda (node)
		   (when (method-defined-p funname node)
		     (funcall function node)))
		 (rete-nodes rete)))

(defun find-nodes-with-items (function rete)
  (rete-find-nodes-if (lambda (node)
			(funcall function (items node)))
		      rete 'items))

(defun is-list-of (type list)
  (and list
       (every (lambda (item)
		(typep item type))
	      list)))

(defun included-in-tokens-p (fact tokens)
  (some (lambda (token)
	  (included-in-p fact token))
	tokens))

(defun nodes-with-tokens-including (fact rete)
  (find-nodes-with-items
   (lambda (items)
     (and (is-list-of 'token items)
	  (included-in-tokens-p fact items)))
   rete))
